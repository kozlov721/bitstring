{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Data.BitString
    ( BitString
    , Bit
    , cons
    , consB
    , uncons
    , unconsB
    , null
    , empty
    , singleton
    , fromByteString
    , fromNumber
    , pack
    , packB
    , fromByteStringWithPadding
    , toByteString
    , toNumber
    , unpack
    , unpackB
    , toByteStringWithPadding
    , drop
    , append
    , splitAt
    , splitAtEnd
    , length
    ) where

import Prelude hiding
    ( drop
    , foldr
    , head
    , init
    , last
    , length
    , map
    , null
    , splitAt
    , tail
    , take
    )

import Control.Applicative.Tools ((<.>))
import Data.Bits
import Data.ByteString.Lazy      (ByteString)
import Data.Int                  (Int64)
import Data.Maybe                (fromJust, isNothing)
import Data.Semigroup            ((<>))
import Data.Word                 (Word8)
import GHC.Exts                  (IsList (..))
import Text.Read

import qualified Data.Bifunctor                as Bi
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Prelude                       as P
import GHC.List (errorEmptyList)


-- | Alias for 'Word8'. /Be cautious to only use/
-- /\(0\) and \(1\) as values, otherwise the correctness is/
-- /not guaranteed!/
type Bit = Word8

-- | Wrapper around lazy 'ByteString'
-- which allows constructing the 'ByteString'
-- from single bits instead of bytes. This can be a useful
-- abstraction when constructing binary data.
data BitString = Empty
               | BitString Word8      -- ^ head
                           Word8      -- ^ number of used bits in the head
                           ByteString -- ^ tail

instance Eq BitString where
  (BitString h1 l1 t1) == (BitString h2 l2 t2) =
      h1 == h2 && l1 == l2 && t1 == t2
  Empty == Empty = True
  _ == _ = False

instance Semigroup BitString where
  (<>) = append

instance Monoid BitString where
  mempty = empty

instance IsList BitString where
  type Item BitString = Bit
  fromList = pack
  toList   = unpack

-- | Binary operators are correctly defined only for bitstrings whose
-- lengths are the same.
instance Bits BitString where
  -- This part could use a little bit of structure
  (.&.) = packZipWithBytes (.&.)
  (.|.) = packZipWithBytes (.|.)
  xor = packZipWith xor
  complement = mapBytes complement
  shift bs x
      | signum x == 1 = append (drop n bs)
          $ const0 n
      | otherwise = append (const0 n)
          $ dropEnd n bs
    where
      n = fromIntegral $ abs x
  rotate bs 0 = bs
  rotate bs x
      | signum x == 1 = (\(x, y) -> append y x) $ splitAt n bs
      | otherwise = (\(x, y) -> append y x) $ splitAtEnd n bs
    where
      n = fromIntegral $ abs x
  bitSize = const maxBound
  bitSizeMaybe = const Nothing
  isSigned = const False
  testBit bs n = (bs .&. bit n) /= pack (repeat 0)
  bit n = cons 1 $ pack $ replicate n 0
  popCount = Data.BitString.foldr (\x y -> y + fromEnum x) 0

instance Show BitString where
  show bs = show (unpack bs)

instance Read BitString where
  readPrec = parens $ prec 10 $ pack <$> readPrec

-- TODO: this can be done faster
const1 :: Int64 -> BitString
const1 n = pack $ replicate (fromIntegral n) 1

const0 :: Int64 -> BitString
const0 n = pack $ replicate (fromIntegral n) 0

-- TODO: better algorithm (kmp probably)
findSubstring :: BitString   -- ^ The string to search for
              -> BitString   -- ^ The string to search in
              -> Maybe Int64 -- ^ The index of the first substring, if exists
findSubstring _ Empty = Nothing
findSubstring p w
    | lookup p w = Just 0
    | otherwise = (+1) <$> findSubstring p (tail w)
  where
    lookup :: BitString -> BitString -> Bool
    lookup _ Empty = False
    lookup Empty _ = True
    lookup p w
        | head p == head w = lookup (tail p) (tail w)
        | otherwise = False

-- | \(\mathcal{O}(1)\) 'cons' is analogous to '(Prelude.:)' for lists.
cons :: Bit -> BitString -> BitString
cons b Empty             = BitString b 1 BL.empty
cons b (BitString h 8 t) = cons b $ BitString 0 0 $ h `BL.cons` t
cons b (BitString h l t) = BitString (h + b * 2 ^ l) (l + 1) t
{-# INLINE cons #-}

-- | \(\mathcal{O}(1)\) Same as 'cons', but takes 'Bool' instead of 'Bit'.
-- In general safer than 'cons' as it eliminates possible
-- mistakes with out-of-range values.
consB :: Bool -> BitString -> BitString
consB b bs = fromIntegral (fromEnum b) `cons` bs
{-# INLINE consB #-}

-- | \(\mathcal{O}(n)\) Similar to 'cons', but append the 'Bit' at the
-- end of the 'BitString'.
snoc :: BitString -> Bit -> BitString
snoc bs b = pack $ unpack bs ++ [b] -- TODO: better implementation
{-# INLINE snoc #-}

-- | \(\mathcal{O}(n)\) Same as 'snoc', but takes 'Bool' as an argument.
snocB :: BitString -> Bool -> BitString
snocB bs b = snoc bs $ fromIntegral $ fromEnum b
{-# INLINE snocB #-}

-- | \(\mathcal{O}(1)\) Returns the first 'Bit' from a 'BitString'.
-- Throws an error in case of an empty 'BitString'.
head :: BitString -> Bit
head Empty             = errorEmptyList "head"
head (BitString _ 0 t) = BL.head t `div` 2 ^ 7
head (BitString h _ _) = h `div` 2 ^ 7
{-# INLINE head #-}

-- | Same as 'head', but returns boolean.
headB :: BitString -> Bool
headB = (/=0) . head
{-# INLINE headB #-}

-- | \(\mathcal{O}(1)\) Returns bits after the head of 'BitString'.
-- An exception is thrown in case of an empty 'BitString'.
tail :: BitString -> BitString
tail = snd . unconsUnsafe
{-# INLINE tail #-}

-- | \(\mathcal{O}(n)\) Returns the length of the 'BitString'.
length :: BitString -> Int64
length Empty             = 0
length (BitString _ l t) = fromIntegral l + 8 * BL.length t
{-# INLINE length #-}

-- | \(\mathcal{O}(1)\) Returns the head and tail of a 'BitString',
-- or 'Nothing' if empty.
uncons :: BitString -> Maybe (Bit, BitString)
uncons Empty = Nothing
uncons (BitString h 1 BLI.Empty) = Just (h, Empty)
uncons (BitString _ 0 t) = do
    (h, t) <- BL.uncons t
    uncons $ BitString h 8 t
uncons (BitString h l t) = Just
    (h `div` 2 ^ (l - 1) `mod` 2, BitString (h .&. complement (2 ^ (l - 1))) (l - 1) t)
{-# INLINE uncons #-}

-- | \(\mathcal{O}(1)\) Same as 'uncons', but returns 'Bool' instead of 'Bit'.
unconsB :: BitString -> Maybe (Bool, BitString)
unconsB = Bi.first (/=0) <.> uncons
{-# INLINE unconsB #-}

unsnoc :: BitString -> Maybe (BitString, Bit)
unsnoc Empty = Nothing
unsnoc bs = Just (pack $ P.init bits, P.last bits)
  where
    bits = unpack bs
{-# INLINE unsnoc #-}

unsnocB :: BitString -> Maybe (BitString, Bool)
unsnocB = Bi.second (/=0) <.> unsnoc
{-# INLINE unsnocB #-}

-- | \(\mathcal{O}(1)\) unsafe version of 'uncons'.
-- Throws an error in case of an empty 'BitString'.
unconsUnsafe :: BitString -> (Bit, BitString)
unconsUnsafe bs = case uncons bs of
    Just x  -> x
    Nothing -> errorEmptyList "unconsUnsafe"
{-# INLINE unconsUnsafe #-}

-- | \(\mathcal{O}(1)\) unsafe version of 'unconsB'.
-- Throws an error in case of an empty 'BitString'.
unconsUnsafeB :: BitString -> (Bool, BitString)
unconsUnsafeB = Bi.first (/=0) . unconsUnsafe
{-# INLINE unconsUnsafeB #-}

-- | \(\mathcal{O}(1)\) unsafe version of 'unsnoc'.
-- Throws an error in case of an empty 'BitString'.
unsnocUnsafe :: BitString -> (BitString, Bit)
unsnocUnsafe bs = case unsnoc bs of
    Just x  -> x
    Nothing -> errorEmptyList "unsnocUnsafe"
{-# INLINE unsnocUnsafe #-}

-- | \(\mathcal{O}(1)\) unsafe version of 'unsnocB'.
-- Throws an error in case of an empty 'BitString'.
unsnocUnsafeB :: BitString -> (BitString, Bool)
unsnocUnsafeB = Bi.second (/=0) . unsnocUnsafe
{-# INLINE unsnocUnsafeB #-}

-- | \(\mathcal{O}(1)\) checks whether 'BitString' is empty or not.
null :: BitString -> Bool
null (BitString _ 0 t) = BL.null t
null _                 = False
{-# INLINE null #-}

-- | \(\mathcal{O}(1)\) constructs an empty 'BitString'.
empty :: BitString
empty = Empty
{-# INLINE empty #-}

-- | \(\mathcal{O}(1)\) converts one bit into a 'BitString'.
singleton :: Bit -> BitString
singleton = (`cons` empty)
{-# INLINE singleton #-}

-- | \(\mathcal{O}(1)\) converts a lazy 'ByteString' into a 'BitString'.
fromByteString :: ByteString -> BitString
fromByteString = BitString 0 0
{-# INLINE fromByteString #-}

fromByteStringStrict :: BS.ByteString -> BitString
fromByteStringStrict = BitString 0 0 . BL.fromStrict
{-# INLINE fromByteStringStrict #-}

-- | \(\mathcal{O}(c)\) reverse of 'toByteStringWithPadding'. Returns 'empty'
-- in case the provided padding is greater than the size of the 'BitString'.
fromByteStringWithPadding :: Word8 -> ByteString -> BitString
fromByteStringWithPadding n bl = drop (fromIntegral n) $ fromByteString bl
{-# INLINE fromByteStringWithPadding #-}

-- | \(\mathcal{O}(n)\) constructs a 'BitString' from a list of 'Bit's.
pack :: [Bit] -> BitString
pack = P.foldr cons empty
{-# INLINE pack #-}

-- | \(\mathcal{O}(n)\) constructs a 'BitString' from a list of 'Bool's.
packB :: [Bool] -> BitString
packB = P.foldr consB empty
{-# INLINE packB #-}

fromNumber :: (Integral a) => a -> BitString
fromNumber = pack . reverse . go
  where
    go :: (Integral a) => a -> [Word8]
    go 0 = []
    go n = (fromIntegral n `mod` 2) : go (n `div` 2)

-- | \(\mathcal{O}(1)\) converts a `BitString` back to `ByteString`.
-- The `BitString` is padded with zeros if its length is not divisible by 8.
toByteString :: BitString -> ByteString
toByteString (BitString h 0 t) = t
toByteString (BitString h 8 t) = h `BL.cons` t
toByteString bs                = toByteString $ 0 `cons` bs
{-# INLINE toByteString #-}

-- | \(\mathcal{O}(1)\) similar to 'toByteString', but also returns the
-- number of leading zeros.
toByteStringWithPadding :: BitString -> (Word8, ByteString)
toByteStringWithPadding Empty                = (0, BL.empty)
toByteStringWithPadding bs@(BitString _ l _) = (8 - l, toByteString bs)
{-# INLINE toByteStringWithPadding #-}

-- | \(\mathcal{O}(n)\) converts a 'BitString' into a list of 'Bit's.
unpack :: BitString -> [Word8]
unpack bs = case uncons bs of
    Nothing     -> []
    Just (h, t) -> h : unpack t
{-# INLINE unpack #-}

-- | \(\mathcal{O}(n)\) converts a 'BitString' into a list of 'Bool's.
unpackB :: BitString -> [Bool]
unpackB = fmap (/=0) . unpack
{-# INLINE unpackB #-}

toNumber :: (Integral a) => BitString -> a
toNumber = P.foldl (\n b -> n * 2 + fromIntegral b) 0 . unpack
{-# INLINE toNumber #-}

-- | \(\mathcal{O}(n)\) returns the suffix of 'BitString' after
-- the first \(n\) elements are dropped, or 'empty' if \(n\) is greater
-- than the length of the 'BitString'.
drop :: Int64 -> BitString -> BitString
drop 0 bs = bs
drop n bs = case uncons bs of
    Nothing     -> empty
    Just (_, t) -> drop (n - 1) t

dropEnd :: Int64 -> BitString -> BitString
dropEnd 0 bs = bs
dropEnd n bs = case unsnoc bs of
    Nothing     -> empty
    Just (t, _) -> dropEnd (n - 1) t

-- | \(\mathcal{O}(n)\) returns the prefix of 'BitString' of length \(n\)
-- or the 'BitString' itself if \(n\) is greater than the length of the
-- 'BitString'.
take :: Int64 -> BitString -> BitString
take 0 bs = empty
take n bs = case uncons bs of
    Nothing     -> bs
    Just (h, t) -> h `cons` take (n - 1) t

takeEnd :: Int64 -> BitString -> BitString
takeEnd 0 bs = empty
takeEnd n bs = case unsnoc bs of
    Nothing     -> bs
    Just (i, l) -> takeEnd (n - 1) i `snoc` l

-- | \(\mathcal{O}(n)\), \(\Omega(n/c)\) appends two 'BitString's.
-- In general not very efficient if length of the latter 'BitString' is not
-- divisible by 8.
append :: BitString -> BitString -> BitString
append (BitString h1 l1 t1) (BitString h2 8 t2) =
    BitString h1 l1 $ BL.append t1 $ h2 `BL.cons` t2
append a b = P.foldr cons b $ unpack a

mapBytes :: (Word8 -> Word8) -> BitString -> BitString
mapBytes _ Empty             = Empty
mapBytes f (BitString h l t) = BitString (f h) l $ BL.map f t
{-# INLINE mapBytes #-}

packZipWith :: (Bool -> Bool -> Bool) -> BitString -> BitString -> BitString
packZipWith f bs1@(BitString h1 l1 t1) bs2@(BitString h2 l2 t2) =
    f b1 b2 `consB` packZipWith f r1 r2
  where
    (b1, r1) = unconsUnsafeB bs1
    (b2, r2) = unconsUnsafeB bs2
packZipWith _ _ _ = Empty

packZipWithBytes :: (Word8 -> Word8 -> Word8)
                 -> BitString
                 -> BitString
                 -> BitString
packZipWithBytes f (BitString h1 l1 t1) (BitString h2 l2 t2) =
    BitString (f h1 h2) (min l1 l2) $ BL.packZipWith f t1 t2
packZipWithBytes _ _ _ = Empty

foldr :: (Bool -> a -> a) -> a -> BitString -> a
foldr _ x Empty = x
foldr f x bs = f h $ Data.BitString.foldr f x t
  where
    (h, t) = unconsUnsafeB bs

splitAt :: Int64 -> BitString -> (BitString , BitString)
splitAt n bs = (take n bs, drop n bs)
{-# INLINE splitAt #-}

splitAtEnd :: Int64 -> BitString -> (BitString, BitString)
splitAtEnd n bs = (dropEnd n bs, takeEnd n bs)
{-# INLINE splitAtEnd #-}

-- Legacy compatibility

{-# DEPRECATED bitString "Use fromByteStringStrict instead" #-}
bitString :: BS.ByteString -> BitString
bitString = fromByteStringStrict
{-# INLINE bitString #-}

{-# DEPRECATED bitStringLazy "Use fromByteString instead" #-}
bitStringLazy :: ByteString -> BitString
bitStringLazy = fromByteString
{-# INLINE bitStringLazy #-}

{-# DEPRECATED toList "Use unpackB instead" #-}
toList :: BitString -> [Bool]
toList = unpackB
{-# INLINE toList #-}

{-# DEPRECATED to01List "Use unpack instead" #-}
to01List :: BitString -> [Word8]
to01List = unpack
{-# INLINE to01List #-}

{-# DEPRECATED fromList "Use packB instead" #-}
fromList :: BitString -> [Bool]
fromList = unpackB
{-# INLINE fromList #-}

{-# DEPRECATED from01List "Use pack instead" #-}
from01List :: BitString -> [Word8]
from01List = unpack
{-# INLINE from01List #-}

