{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.BitString
-- Copyright   : (c) Martin Kozlovsky 2022
-- License     : BSD-style
--
-- Stability   : unstable
-- Portability : portable
--
-- A simple and convenient implementation of bit vectors using a wrapper
-- around 'Data.ByteString.Lazy'.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.BitString as BS
--
--

module Data.BitString
    ( -- * @BitString@
      BitString
    , Bit
      -- * Basic interface
    , cons
    , consB
    , uncons
    , unconsUnsafe
    , unconsB
    , unconsUnsafeB
    , snoc
    , snocB
    , unsnoc
    , unsnocUnsafe
    , unsnocB
    , unsnocUnsafeB
    , append
    , head
    , headB
    , tail
    , init
    , last
    , lastB
    , null
    , length
      -- * Construction and deconstruction of 'BitString's
    , empty
    , singleton
    , fromByteString
    , fromByteStringStrict
    , fromByteStringWithPadding
    , fromNumber
    , pack
    , packB
    , toByteString
    , toByteStringStrict
    , toByteStringWithPadding
    , toNumber
    , unpack
    , unpackB
      -- * Breaking 'BitString's
    , take
    , drop
    , takeEnd
    , dropEnd
    , splitAt
    , splitAtEnd
      -- * Combining 'BitString's
    , zip
    , zipB
    , zipWith
    , packZipWith
      -- * Indexing
    , findSubstring
      -- * Files
    , readFile
    , writeFile
    , appendFile
      -- * Legacy compatibility
    , bitString
    , bitStringLazy
    , Data.BitString.toList
    , to01List
    , Data.BitString.fromList
    , from01List
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
    , zip
    , zipWith
    , replicate
    , readFile
    , appendFile
    , writeFile
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
-- /\(0\) and \(1\) as values, otherwise the behavior is/
-- /undefined./
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

-- | Binary operators are correctly defined only for 'BitString's whose
-- lengths are the same.
instance Bits BitString where
  -- TODO: This part could use a little bit of structure
  (.&.) = packZipWithBytes (.&.)
  (.|.) = packZipWithBytes (.|.)
  xor = packZipWithBytes xor
  complement = mapBytes complement
  shift bs x
      | signum x == 1 = append (drop n bs)
          $ replicate n False
      | otherwise = append (replicate n False)
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
  bit n = cons 1 $ replicate (fromIntegral n) False
  popCount = foldr (\x y -> y + fromEnum x) 0

instance Show BitString where
  show bs = show (unpack bs)

instance Read BitString where
  readPrec = parens $ prec 10 $ pack <$> readPrec

-- \(\mathcal{O}(n)\) Creates a 'BitString' of length \(n\) filled
-- with the given value.
replicate :: Int64 -> Bool -> BitString
replicate n b = packB $ P.replicate (fromIntegral n) b

-- TODO: better algorithm (kmp probably)
-- | \(\mathcal{O}(n \cdot m)\) Locates a substring in a 'BitString'.
-- Returns the index of the first match, or Nothing if there is no match.
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
-- mistakes with undefined values.
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

-- | \(\mathcal{O}(1)\) Same as 'head', but returns boolean.
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

-- | \(\mathcal{O}(1)\) Returns 'head' and 'tail' of a 'BitString',
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

-- | \(\mathcal{O}(1)\) Same as 'uncons', but uses 'Bool' instead of 'Bit'.
unconsB :: BitString -> Maybe (Bool, BitString)
unconsB = Bi.first (/=0) <.> uncons
{-# INLINE unconsB #-}

-- | \(\mathcal{O}(n)\) Returns 'init' and 'last' of a 'BitString',
-- or 'Nothing' if empty.
unsnoc :: BitString -> Maybe (BitString, Bit)
unsnoc Empty = Nothing
unsnoc bs = Just (pack $ P.init bits, P.last bits)
  where
    bits = unpack bs
{-# INLINE unsnoc #-}

-- | \(\mathcal{O}(n)\) Same as 'unsnoc', but uses 'Bool' instead of 'Bit'.
unsnocB :: BitString -> Maybe (BitString, Bool)
unsnocB = Bi.second (/=0) <.> unsnoc
{-# INLINE unsnocB #-}

-- | \(\mathcal{O}(n)\) Returns a 'BitString' without the last element.
init :: BitString -> BitString
init = fst . unsnocUnsafe

-- | \(\mathcal{O}(n)\) Returns the last element of a 'BitString' as 'Bit'.
last :: BitString -> Bit
last = snd . unsnocUnsafe

-- | \(\mathcal{O}(n)\) Boolean equivalent for 'last'.
lastB :: BitString -> Bool
lastB = (/=0) . last

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

-- | \(\mathcal{O}(1)\) converts a lazy 'ByteString' into a 'BitString'.
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

-- | Converts an instance of 'Integral' to a 'BitString'. Be aware that no
-- padding is added for fixe sized integrals.
fromNumber :: (Integral a) => a -> BitString
fromNumber = pack . reverse . go
  where
    go :: (Integral a) => a -> [Word8]
    go 0 = []
    go n = (fromIntegral n `mod` 2) : go (n `div` 2)

-- | \(\mathcal{O}(1)\) Converts a 'BitString' to a lazy 'ByteString'.
-- The 'BitString' is prepended with zeros if its length is not divisible by 8.
toByteString :: BitString -> ByteString
toByteString (BitString h 0 t) = t
toByteString (BitString h 8 t) = h `BL.cons` t
toByteString bs                = toByteString $ 0 `cons` bs
{-# INLINE toByteString #-}

-- | \(\mathcal{O}(1)\) Converts a 'BitString' to a strict 'ByteString'.
-- The 'BitString' is prepended with zeros if its length is not divisible by 8.
toByteStringStrict :: BitString -> BS.ByteString
toByteStringStrict = BL.toStrict . toByteString
{-# INLINE toByteStringStrict #-}

-- | \(\mathcal{O}(1)\) Similar to 'toByteString', but also returns the
-- number of leading zeros.
toByteStringWithPadding :: BitString -> (Word8, ByteString)
toByteStringWithPadding Empty                = (0, BL.empty)
toByteStringWithPadding bs@(BitString _ l _) = (8 - l, toByteString bs)
{-# INLINE toByteStringWithPadding #-}

-- | \(\mathcal{O}(n)\) Converts a 'BitString' into a list of 'Bit's.
unpack :: BitString -> [Word8]
unpack bs = case uncons bs of
    Nothing     -> []
    Just (h, t) -> h : unpack t
{-# INLINE unpack #-}

-- | \(\mathcal{O}(n)\) Converts a 'BitString' into a list of 'Bool's.
unpackB :: BitString -> [Bool]
unpackB = fmap (/=0) . unpack
{-# INLINE unpackB #-}

-- | \(\mathcal{O}(n)\) Converts a 'BitString' into 'Integral'.
toNumber :: (Integral a) => BitString -> a
toNumber = P.foldl (\n b -> n * 2 + fromIntegral b) 0 . unpack
{-# INLINE toNumber #-}

-- | \(\mathcal{O}(n)\) Returns the suffix of 'BitString' after
-- the first \(n\) elements are dropped, or 'empty' if \(n\) is greater
-- than the length of the 'BitString'.
drop :: Int64 -> BitString -> BitString
drop 0 bs = bs
drop n bs = case uncons bs of
    Nothing     -> empty
    Just (_, t) -> drop (n - 1) t

-- TODO: Better algorithm, make use of ByteString
-- | \(\mathcal{O}(n \cdot m)\) Drops \(n\) elemnets
-- from the end of the 'BitString'. Inefficient.
dropEnd :: Int64 -> BitString -> BitString
dropEnd 0 bs = bs
dropEnd n bs = case unsnoc bs of
    Nothing     -> empty
    Just (t, _) -> dropEnd (n - 1) t

-- | \(\mathcal{O}(n)\) Returns the prefix of 'BitString' of length \(n\)
-- or the 'BitString' itself if \(n\) is greater than the length of the
-- 'BitString'.
take :: Int64 -> BitString -> BitString
take 0 bs = empty
take n bs = case uncons bs of
    Nothing     -> bs
    Just (h, t) -> h `cons` take (n - 1) t

-- TODO: Better algorithm, make use of ByteString
-- | \(\mathcal{O}(n \cdot m)\) Takes \(n\) elemnets
-- from the end of the 'BitString'. Inefficient.
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
{-# INLINE append #-}

mapBytes :: (Word8 -> Word8) -> BitString -> BitString
mapBytes _ Empty             = Empty
mapBytes f (BitString h l t) = BitString (f h) l $ BL.map f t
{-# INLINE mapBytes #-}

-- | \(\mathcal{O}(\min(n, m))\) Takes two 'BitStrings' and returns a list
-- of pairs of 'Bit's. If one 'BitString' is longer, its last elements are
-- discarded.
zip :: BitString -> BitString -> [(Word8, Word8)]
zip Empty _ = []
zip _ Empty = []
zip x y = (head x, head y) : zip (tail x) (tail y)

-- | \(\mathcal{O}(\min(n, m))\) Same as 'zip', but returns a list of pairs
-- of 'Bool's.
zipB :: BitString -> BitString -> [(Bool, Bool)]
zipB Empty _ = []
zipB _ Empty = []
zipB x y = (headB x, headB y) : zipB (tail x) (tail y)

-- I don't see much use, but why not.
-- | \(\mathcal{O}(\min(n, m))\) Equivalent of 'Prelude.zipWith'
-- for 'BitString's.
zipWith :: (Bool -> Bool -> a) -> BitString -> BitString -> [a]
zipWith _ Empty _ = []
zipWith _ _ Empty = []
zipWith f x y = f (headB x) (headB y) : zipWith f (tail x) (tail y)

-- | \(\mathcal{O}(\min(n, m))\) Generalized version of 'zipWith'.
-- Takes two 'BitStrings' and a binary function on 'Bool's and returns new
-- 'BitString'.
packZipWith :: (Bool -> Bool -> Bool) -> BitString -> BitString -> BitString
packZipWith _ Empty _ = Empty
packZipWith _ _ Empty = Empty
packZipWith f x y = f (headB x) (headB y) `consB` packZipWith f (tail x) (tail y)

packZipWithBytes :: (Word8 -> Word8 -> Word8)
                 -> BitString
                 -> BitString
                 -> BitString
packZipWithBytes f (BitString h1 l1 t1) (BitString h2 l2 t2) =
    BitString (f h1 h2) (min l1 l2) $ BL.packZipWith f t1 t2
packZipWithBytes _ _ _ = Empty

-- \(\mathcal{O}(n)\) Equivalent of 'Prelude.foldr'.
foldr :: (Bool -> a -> a) -> a -> BitString -> a
foldr _ x Empty = x
foldr f x bs = f h $ foldr f x t
  where
    (h, t) = unconsUnsafeB bs

-- | \(\mathcal{O}(n)\) @'splitAt' n xs@ is equivalent
-- to @('take' n xs, 'drop' n xs)@.
splitAt :: Int64 -> BitString -> (BitString , BitString)
splitAt n bs = (take n bs, drop n bs)
{-# INLINE splitAt #-}

-- | \(\mathcal{O}(n \cdot m)\) @'splitAtEnd' n xs@ is equivalent
-- to @('dropEnd' n xs, 'takeEnd' n xs)@.
splitAtEnd :: Int64 -> BitString -> (BitString, BitString)
splitAtEnd n bs = (dropEnd n bs, takeEnd n bs)
{-# INLINE splitAtEnd #-}

-- | Read a file into a 'BitString'.
readFile :: FilePath -> IO BitString
readFile = fromByteString <.> BL.readFile

-- | Writes a 'BitString' to a file.
writeFile :: FilePath -> BitString -> IO ()
writeFile p = BL.writeFile p . toByteString

-- | Appends a 'BitString' to a file.
appendFile :: FilePath -> BitString -> IO ()
appendFile p = BL.appendFile p . toByteString

-- Legacy compatibility

{-# DEPRECATED bitString "Use fromByteStringStrict instead" #-}
-- | Alias to 'fromByteStringStrict'.
bitString :: BS.ByteString -> BitString
bitString = fromByteStringStrict
{-# INLINE bitString #-}

{-# DEPRECATED bitStringLazy "Use fromByteString instead" #-}
-- | Alias to 'fromByteString'.
bitStringLazy :: ByteString -> BitString
bitStringLazy = fromByteString
{-# INLINE bitStringLazy #-}

{-# DEPRECATED toList "Use unpackB instead" #-}
-- | Alias to 'unpackB'.
toList :: BitString -> [Bool]
toList = unpackB
{-# INLINE toList #-}

{-# DEPRECATED to01List "Use unpack instead" #-}
-- | Alias to 'unpack'.
to01List :: BitString -> [Word8]
to01List = unpack
{-# INLINE to01List #-}

{-# DEPRECATED fromList "Use packB instead" #-}
-- | Alias to 'packB'.
fromList :: [Bool] -> BitString
fromList = packB
{-# INLINE fromList #-}

{-# DEPRECATED from01List "Use pack instead" #-}
-- | Alias to 'pack'.
from01List :: [Word8] -> BitString
from01List = pack
{-# INLINE from01List #-}

