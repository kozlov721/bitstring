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
    , pack
    , packB
    , fromByteStringWithPadding
    , fromByteStringPadded
    , toByteString
    , unpack
    , unpackB
    , toByteStringWithPadding
    , toByteStringPadded
    , drop
    , append
    ) where

import Prelude hiding
        ( head
        , tail
        , last
        , init
        , null
        , length
        , map
        , take
        , drop
        )

import Control.Applicative.Tools ((<.>))
import Data.Bits
import Data.ByteString.Lazy      (ByteString)
import Data.Semigroup            ((<>))
import Data.Int                  (Int64)
import Data.Maybe                (fromJust)
import Data.Word                 (Word8)
import GHC.Exts                  (IsList(..))

import qualified Data.Bifunctor       as Bi
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI


-- | Alias for 'Word8'. /Be cautious to only use/
-- /\(0\) and \(1\) as values, otherwise the correctness is/
-- /not guaranteed!/
type Bit = Word8

-- | Wrapper around lazy 'ByteString'
-- which allows constructing the 'ByteString'
-- from single bits instead of bytes. This can be a useful
-- abstraction when constructing binary data.
data BitString = Empty
               | BitString
    Word8      -- ^ head
    Word8      -- ^ number of used bits in the head
    ByteString -- ^ tail
    deriving (Show)

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

instance Bits BitString where
  (.&.) = packZipWith (.&.)
  (.|.) = packZipWith (.|.)
  xor = packZipWith xor
  complement = map not
  shift bs 0 = bs
  shift bs n
    | signum n == -1 = drop (fromIntegral n) bs
    | otherwise = Prelude.foldr cons bs $ replicate n 0
  rotate a b = a
  bitSize = fromIntegral . length
  bitSizeMaybe = Just . fromIntegral . length
  isSigned = const False
  testBit _ _ = True
  bit n = cons 1 $ pack $ replicate (n - 1) 0
  popCount = Data.BitString.foldr (\x y -> y + fromEnum x) 0


-- | \(\mathcal{O}(1)\) 'cons' is analogous to '(Prelude.:)' for lists.
cons :: Bit -> BitString -> BitString
cons b Empty = BitString (b * 2 ^ 7) 1 BL.empty
cons b (BitString h 8 t) = cons b $ BitString 0 0 $ h `BL.cons` t
cons b (BitString h l t) = BitString (h `div` 2 + b * 2 ^ 7) (l + 1) t

-- | \(\mathcal{O}(1)\) Same as 'cons', but takes 'Bool' instead of 'Bit'.
-- In general safer than 'cons' as it eliminates possible
-- mistakes with out-of-range values.
consB :: Bool -> BitString -> BitString
consB b bs = fromIntegral (fromEnum b) `cons` bs

-- | \(\mathcal{O}(1)\) Returns the first 'Bit' from a 'BitString'.
-- Throws an error in case of an empty 'BitString'.
head :: BitString -> Bit
head Empty = error "empty BitString"
head (BitString _ 0 t) = BL.head t `div` 2 ^ 7
head (BitString h _ _) = h `div` 2 ^ 7

-- | Same as 'head', but returns boolean.
headB :: BitString -> Bool
headB = (/=0) . head

-- | \(\mathcal{O}(1)\) Returns bits after the head of 'BitString'.
-- An exception is thrown in case of an empty 'BitString'.
tail :: BitString -> BitString
tail = snd . unconsUnsafe

-- | \(\mathcal{O}(n)\) Returns the length of the 'BitString'.
length :: BitString -> Int64
length Empty = 0
length (BitString _ l t) = fromIntegral l + BL.length t

-- | \(\mathcal{O}(1)\) Returns the head and tail of a 'BitString',
-- or 'Nothing' if empty.
uncons :: BitString -> Maybe (Bit, BitString)
uncons Empty = Nothing
uncons (BitString h 1 BLI.Empty) = Just (h `div` 2 ^ 7, Empty)
uncons (BitString _ 0 t) = do
    (h, t) <- BL.uncons t
    uncons $ BitString h 8 t
uncons (BitString h l t) = Just
    (h `div` 2 ^ 7, BitString (h * 2) (l - 1) t)

-- | \(\mathcal{O}(1)\) Same as 'uncons', but returns 'Bool' instead of 'Bit'.
unconsB :: BitString -> Maybe (Bool, BitString)
unconsB = Bi.first (/=0) <.> uncons

-- | \(\mathcal{O}(1)\) unsafe version of 'uncons'.
-- Throws an error in case of an empty 'BitString'.
unconsUnsafe :: BitString -> (Bit, BitString)
unconsUnsafe bs = case uncons bs of
    Just x  -> x
    Nothing -> error "empty BitString"

-- | \(\mathcal{O}(1)\) unsafe version of 'unconsB'.
-- Throws an error in case of an empty 'BitString'.
unconsUnsafeB :: BitString -> (Bool, BitString)
unconsUnsafeB = Bi.first (/=0) . unconsUnsafe

-- | \(\mathcal{O}(1)\) checks whether 'BitString' is empty or not.
null :: BitString -> Bool
null (BitString _ 0 t) = BL.null t
null _                 = False

-- | \(\mathcal{O}(1)\) constructs an empty 'BitString'.
empty :: BitString
empty = Empty

-- | \(\mathcal{O}(1)\) converts one bit into a 'BitString'.
singleton :: Bit -> BitString
singleton = (`cons` empty)

-- | \(\mathcal{O}(1)\) converts a lazy 'ByteString' into a 'BitString'.
fromByteString :: ByteString -> BitString
fromByteString = BitString 0 0

-- | \(\mathcal{O}(c)\) reverse of 'toByteStringWithPadding'. Returns 'empty'
-- in case the provided padding is greater than the size of the 'BitString'.
fromByteStringWithPadding :: Word8 -> ByteString -> BitString
fromByteStringWithPadding n bl = drop (fromIntegral n) $ fromByteString bl

-- | \(\mathcal{O}(c)\) reverse of 'toByteStringPadded'. Returns 'empty'
-- in case the padding denoted by the first byte
-- is greater than the size of the 'BitString'.
fromByteStringPadded :: ByteString -> BitString
fromByteStringPadded bl
    | BL.null bl = empty
    | otherwise = uncurry fromByteStringWithPadding
        $ fromJust
        $ BL.uncons bl

-- | \(\mathcal{O}(n)\) constructs a 'BitString' from a list of 'Bit's.
pack :: [Bit] -> BitString
pack = Prelude.foldr cons empty

-- | \(\mathcal{O}(n)\) constructs a 'BitString' from a list of 'Bool's.
packB :: [Bool] -> BitString
packB = Prelude.foldr consB empty

-- | \(\mathcal{O}(1)\) converts a `BitString` back to `ByteString`.
-- The `BitString` is padded with zeros if its length is not divisible by 8.
toByteString :: BitString -> ByteString
toByteString (BitString h 0 t) = t
toByteString (BitString h 8 t) = h `BL.cons` t
toByteString bs                = toByteString $ 0 `cons` bs

-- | \(\mathcal{O}(1)\) similar to 'toByteString', but also returns the
-- number of leading zeros.
toByteStringWithPadding :: BitString -> (Word8, ByteString)
toByteStringWithPadding Empty = (0, BL.empty)
toByteStringWithPadding bs@(BitString _ l _) = (8 - l, toByteString bs)

-- | \(\mathcal{O}(1)\) similar to 'toByteStringWithPadding',
-- but prepends the final 'ByteString' with the number of leading zeros.
--
-- Equivalent to:
--
-- > uncurry ByteString.cons . toByteStringWithPadding
toByteStringPadded :: BitString -> ByteString
toByteStringPadded = uncurry BL.cons . toByteStringWithPadding

-- | \(\mathcal{O}(n)\) converts a 'BitString' into a list of 'Bit's.
unpack :: BitString -> [Word8]
unpack bs = case uncons bs of
    Nothing      -> []
    Just (h, t)  -> h : unpack t

-- | \(\mathcal{O}(n)\) converts a 'BitString' into a list of 'Bool's.
unpackB :: BitString -> [Bool]
unpackB = fmap (/=0) . unpack

-- | \(\mathcal{O}(n)\) returns the suffix of 'BitString' after
-- the first \(n\) elements are dropped, or 'empty' if \(n\) is greater
-- than the length of the 'BitString'.
drop :: Int64 -> BitString -> BitString
drop 0 bs = bs
drop n bs = case uncons bs of
    Nothing      -> empty
    Just (_, t)  -> drop (n - 1) t

-- | \(\mathcal{O}(n)\) returns the prefix of 'BitString' of length \(n\)
-- or the 'BitString' itself if \(n\) is greater than the length of the
-- 'BitString'.
take :: Int64 -> BitString -> BitString
take 0 bs = empty
take n bs = case uncons bs of
    Nothing     -> bs
    Just (h, t) -> h `cons` take (n - 1) t

-- | \(\mathcal{O}(n)\), \(\Omega(n/c)\) appends two 'BitString's.
-- In general not very efficient if length of the latter 'BitString' is not
-- divisible by 8.
append :: BitString -> BitString -> BitString
append (BitString h1 l1 t1) (BitString h2 8 t2) =
    BitString h1 l1 $ BL.append t1 $ h2 `BL.cons` t2
append a b = Prelude.foldr cons b (reverse (unpack a))

map :: (Bool -> Bool) -> BitString -> BitString
map f bs = case unconsB bs of
    Nothing     -> empty
    Just (h, t) -> f h `consB` map f t

packZipWith :: (Bool -> Bool -> Bool) -> BitString -> BitString -> BitString
packZipWith f bs1@(BitString h1 l1 t1) bs2@(BitString h2 l2 t2) =
    f b1 b2 `consB` packZipWith f r1 r2
  where
    (b1, r1) = unconsUnsafeB bs1
    (b2, r2) = unconsUnsafeB bs2
packZipWith _ _ _ = Empty

foldr :: (Bool -> a -> a) -> a -> BitString -> a
foldr _ x Empty = x
foldr f x bs = f h $ Data.BitString.foldr f x t
  where
    (h, t) = unconsUnsafeB bs
