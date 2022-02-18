{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

-- |
#ifdef BIGENDIAN
#define MODULE Data.BitString.BigEndian
-- Module      : Data.BitString.BigEndian
#else
#define MODULE Data.BitString
-- Module      : Data.BitString
#endif
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
-- clashes with "Prelude" functions, e.g.
--
#ifdef BIGENDIAN
-- > import qualified Data.BitString.BigEndian as BSE
#else
-- > import qualified Data.BitString as BS
#endif
--
--

module MODULE
    ( -- * @BitString@
      BitString
    , Bit
      -- * Basic interface
    , cons
    , consB
    , pattern (:::)
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
    , concat
    , head
    , headB
    , tail
    , init
    , last
    , lastB
    , null
    , length
    , reverse
    , paddEqual
      -- * Construction and deconstruction of 'BitString's
    , empty
    , pattern Empty
    , singleton
    , fromByteString
    , fromByteStringStrict
    , fromByteStringPadded
    , fromNumber
    , pack
    , packB
    , replicate
    , toByteString
    , toByteStringStrict
    , toByteStringPadded
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
    , (!)
    , (!?)
    , findSubstring
      -- * Reducing 'BitString's
    , foldr
    , foldr'
    , foldl
    , foldl'
      -- * Files
    , readFile
    , writeFile
    , appendFile
      -- * Legacy compatibility
    , bitString
    , bitStringLazy
    , unsafeBitString'
    , MODULE.toList
    , to01List
    , MODULE.fromList
    , from01List
    , realizeBitStringStrict
    , realizeBitStringLazy
    ) where

import Prelude hiding
    ( appendFile
    , concat
    , drop
    , foldl
    , foldr
    , head
    , init
    , last
    , length
    , lookup
    , map
    , null
    , readFile
    , replicate
    , reverse
    , splitAt
    , tail
    , take
    , writeFile
    , zip
    , zipWith
    )

import Data.Bits
import Data.ByteString.Lazy      (ByteString)
import Data.Int                  (Int64)
import Data.Maybe                (fromJust)
import Data.Word                 (Word8)
import GHC.Exts                  (IsList (..))
import GHC.List                  (errorEmptyList)
import Text.Read

import qualified Data.Bifunctor                as Bi
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Language.Haskell.TH.Syntax    as TH
import qualified Prelude                       as P


-- | Alias for 'Word8'. /Be cautious to only use/
-- /\(0\) and \(1\) as values, otherwise the behavior is/
-- /undefined./
type Bit = Word8

-- | Wrapper around lazy 'ByteString'
-- which allows constructing the 'ByteString'
-- from single bits instead of bytes. This can be a useful
-- abstraction when constructing binary data.
data BitString = BitString Word8      -- ^ head
                           Word8      -- ^ number of used bits in the head
                           ByteString -- ^ tail
               deriving (TH.Lift)

instance Eq BitString where
  (BitString h1 l1 t1) == (BitString h2 l2 t2) =
      h1 == h2 && l1 == l2 && t1 == t2

instance Ord BitString where
  compare Empty Empty = EQ
  compare Empty _     = LT
  compare _     Empty = GT
  compare l     r     = compare (toByteString l) (toByteString r)

instance Semigroup BitString where
  (<>) = append

instance Monoid BitString where
  mempty = empty

instance IsList BitString where
  type Item BitString = Bit
  fromList = pack
  toList   = unpack

instance Bits BitString where
  x .&. y = uncurry (packZipWithBytes (.&.)) $ paddEqual x y
  x .|. y = uncurry (packZipWithBytes (.|.)) $ paddEqual x y
  x `xor` y = uncurry (packZipWithBytes xor) $ paddEqual x y
  complement = mapBytes complement

  shift bs x
      | signum x == 1 = append (drop n bs) $ replicate n False
      | otherwise = append (replicate n False) $ dropEnd n bs
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
  testBit bs n = reverse bs !? fromIntegral n == Just 1
  bit n = cons 1 $ replicate (fromIntegral n) False
  popCount = foldr ((+) . fromEnum) 0

instance Show BitString where
  show bs = show (unpack bs)

instance Read BitString where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ pack <$> readPrec
#else
  readsPrec p = readParen (p > 10) $ \r -> do
      (xs, t) <- reads r
      return (pack xs, t)
#endif

-- \(\mathcal{O}(n)\) Creates a 'BitString' of length \(n\) filled
-- with the given value.
replicate :: Int64 -> Bool -> BitString
replicate n b = packB $ P.replicate (fromIntegral n) b

-- \(\mathcal{O}(n)\) Reverses elements in a 'BitString'.
reverse :: BitString -> BitString
reverse = pack . P.reverse . unpack

{-# Complete Empty, (:::) #-}
-- | Pattern synonym for an empty 'BitString'.
pattern Empty :: BitString
pattern Empty = BitString 0 0 BLI.Empty

-- | \(\mathcal{O}(1)\) Pattern synonym for 'cons'.
infixr 5 :::
pattern (:::) :: Word8 -> BitString -> BitString
pattern b:::bs <- (uncons -> Just (b, bs))
  where b:::bs = cons b bs

-- | \(\mathcal{O}(n)\) Safe version of '(!)'.
infixl 9 !?
(!?) :: BitString -> Int64 -> Maybe Bit
(!?) Empty _ = Nothing
(!?) bs 0    = Just $ head bs
(!?) bs n    = tail bs !? (n - 1)

-- | \(\mathcal{O}(n)\) Gets nth bit from a 'BitString'. Throws an error
-- in case of out-of-range index.
infixl 9 !
(!) :: BitString -> Int64 -> Bit
(!) Empty _ = errorEmptyList "(!)"
(!) bs n    = fromJust $ bs !? n

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
    lookup Empty _ = True
    lookup _ Empty = False
    lookup (p:::ps) (w:::ws)
        | p == w    = lookup ps ws
        | otherwise = False

infixr 5 `cons`, `consB`
infixl 5 `snoc`, `snocB`

-- | \(\mathcal{O}(1)\) 'cons' is analogous to '(Prelude.:)' for lists.
cons :: Bit -> BitString -> BitString
cons b (BitString h l t)
    | l == 7    = BitString 0 0 (push h b 7 `BL.cons` t)
    | otherwise = BitString (push h b l) (l + 1) t
  where
#ifdef BIGENDIAN
    push h b _ = h * 2 + b
#else
    push h b l = h `div` 2 + b * 2 ^ 7
#endif
{-# INLINE cons #-}

-- | \(\mathcal{O}(1)\) Same as 'cons', but takes 'Bool' instead of 'Bit'.
-- In general safer than 'cons' as it eliminates possible
-- mistakes with undefined values.
consB :: Bool -> BitString -> BitString
consB b bs = fromIntegral (fromEnum b) ::: bs
{-# INLINE consB #-}

-- | \(\mathcal{O}(n)\) Similar to 'cons', but appends the 'Bit' at the
-- end of the 'BitString'. Very inefficient as it requires rebuilding
-- the whole 'BitString'.
snoc :: BitString -> Bit -> BitString
snoc bs b = pack $ unpack bs ++ [b]
{-# INLINE snoc #-}

-- | \(\mathcal{O}(n)\) Same as 'snoc', but takes 'Bool' as an argument.
snocB :: BitString -> Bool -> BitString
snocB bs b = snoc bs $ fromIntegral $ fromEnum b
{-# INLINE snocB #-}

-- | \(\mathcal{O}(1)\) Returns the first 'Bit' from a 'BitString'.
-- Throws an error in case of an empty 'BitString'.
head :: BitString -> Bit
head Empty   = errorEmptyList "head"
head (b:::_) = b
{-# INLINE head #-}

-- | \(\mathcal{O}(1)\) Same as 'head', but returns boolean.
headB :: BitString -> Bool
headB = (/=0) . head
{-# INLINE headB #-}

-- | \(\mathcal{O}(1)\) Returns bits after the head of 'BitString'.
-- An exception is thrown in case of an empty 'BitString'.
tail :: BitString -> BitString
tail Empty    = errorEmptyList "tail"
tail (_:::bs) = bs
{-# INLINE tail #-}

-- | \(\mathcal{O}(n)\) Returns the length of the 'BitString'.
length :: BitString -> Int64
length Empty             = 0
length (BitString _ l t) = fromIntegral l + 8 * BL.length t
{-# INLINE length #-}

-- | \(\mathcal{O}(n)\) Takes two 'BitString's and returns a tuple
-- of two 'BitStrings', where the shorter one is padded with zeros so
-- its legnth match the longer one.
paddEqual :: BitString -> BitString -> (BitString, BitString)
paddEqual Empty Empty = (Empty, Empty)
paddEqual Empty bs    = (mapBytes (const 0) bs, bs)
paddEqual bs    Empty = (mapBytes (const 0) bs, bs)
paddEqual x y
    | length x > length y = (x, p <> y)
    | length y > length x = (p <> x, y)
    | otherwise           = (x, y)
  where
    p = replicate n False
    n = abs $ length x - length y

-- | \(\mathcal{O}(1)\) Returns 'head' and 'tail' of a 'BitString',
-- or 'Nothing' if empty.
uncons :: BitString -> Maybe (Bit, BitString)
uncons Empty = Nothing
uncons (BitString _ 0 t) = do
    (h, t) <- BL.uncons t
    uncons $ BitString h 8 t
uncons (BitString h l t) = Just
#ifdef BIGENDIAN
    ( h `mod` 2, BitString (h `div` 2) (l - 1) t)
#else
    ( h `div` 2 ^ 7, BitString (h * 2) (l - 1) t)
#endif
{-# INLINE uncons #-}

-- | \(\mathcal{O}(1)\) Same as 'uncons', but uses 'Bool' instead of 'Bit'.
unconsB :: BitString -> Maybe (Bool, BitString)
unconsB bs = Bi.first (/=0) <$> uncons bs
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
unsnocB bs = Bi.second (/=0) <$> unsnoc bs
{-# INLINE unsnocB #-}

-- | \(\mathcal{O}(n)\) Returns a 'BitString' without the last element.
init :: BitString -> BitString
init = fst . unsnocUnsafe
{-# INLINE init #-}

-- | \(\mathcal{O}(n)\) Returns the last element of a 'BitString' as 'Bit'.
last :: BitString -> Bit
last = snd . unsnocUnsafe
{-# INLINE last #-}

-- | \(\mathcal{O}(n)\) Boolean equivalent for 'last'.
lastB :: BitString -> Bool
lastB = (/=0) . last
{-# INLINE lastB #-}

-- | \(\mathcal{O}(1)\) Unsafe version of 'uncons'.
-- Throws an error in case of an empty 'BitString'.
unconsUnsafe :: BitString -> (Bit, BitString)
unconsUnsafe Empty    = errorEmptyList "unconsUnsafe"
unconsUnsafe (b:::bs) = (b, bs)
{-# INLINE unconsUnsafe #-}

-- | \(\mathcal{O}(1)\) Unsafe version of 'unconsB'.
-- Throws an error in case of an empty 'BitString'.
unconsUnsafeB :: BitString -> (Bool, BitString)
unconsUnsafeB = Bi.first (/=0) . unconsUnsafe
{-# INLINE unconsUnsafeB #-}

-- | \(\mathcal{O}(1)\) Unsafe version of 'unsnoc'.
-- Throws an error in case of an empty 'BitString'.
unsnocUnsafe :: BitString -> (BitString, Bit)
unsnocUnsafe bs = case unsnoc bs of
    Just x  -> x
    Nothing -> errorEmptyList "unsnocUnsafe"
{-# INLINE unsnocUnsafe #-}

-- | \(\mathcal{O}(1)\) Unsafe version of 'unsnocB'.
-- Throws an error in case of an empty 'BitString'.
unsnocUnsafeB :: BitString -> (BitString, Bool)
unsnocUnsafeB = Bi.second (/=0) . unsnocUnsafe
{-# INLINE unsnocUnsafeB #-}

-- | \(\mathcal{O}(1)\) Checks whether 'BitString' is empty or not.
null :: BitString -> Bool
null (BitString _ 0 t) = BL.null t
null _                 = False
{-# INLINE null #-}

-- | \(\mathcal{O}(1)\) Constructs an empty 'BitString'.
empty :: BitString
empty = Empty
{-# INLINE empty #-}

-- | \(\mathcal{O}(1)\) Converts one bit into a 'BitString'.
singleton :: Bit -> BitString
singleton = (:::empty)
{-# INLINE singleton #-}

-- | \(\mathcal{O}(1)\) Converts a lazy 'ByteString' into a 'BitString'.
fromByteString :: ByteString -> BitString
fromByteString = BitString 0 0
{-# INLINE fromByteString #-}

-- | \(\mathcal{O}(1)\) Converts a lazy 'ByteString' into a 'BitString'.
fromByteStringStrict :: BS.ByteString -> BitString
fromByteStringStrict = BitString 0 0 . BL.fromStrict
{-# INLINE fromByteStringStrict #-}

-- | \(\mathcal{O}(c)\) Reverse of 'toByteStringPadded'. Returns 'empty'
-- in case the provided padding is greater than the size of the 'BitString'.
fromByteStringPadded :: Word8 -> ByteString -> BitString
fromByteStringPadded n bl = drop (fromIntegral n) $ fromByteString bl
{-# INLINE fromByteStringPadded #-}

-- | \(\mathcal{O}(n)\) Constructs a 'BitString' from a list of 'Bit's.
pack :: [Bit] -> BitString
pack = P.foldr cons empty
{-# INLINE pack #-}

-- | \(\mathcal{O}(n)\) Constructs a 'BitString' from a list of 'Bool's.
packB :: [Bool] -> BitString
packB = P.foldr consB empty
{-# INLINE packB #-}

-- | \(\mathcal{O}(\log n)\) Converts an instance of
-- 'Integral' to a 'BitString'. Be aware that no padding
-- is added for fixed sized integrals.
fromNumber :: (Integral a) => a -> BitString
fromNumber = pack . P.reverse . go
  where
    go :: (Integral a) => a -> [Word8]
    go 0 = []
    go n = (fromIntegral n `mod` 2) : go (n `div` 2)
{-# INLINE fromNumber #-}

-- | \(\mathcal{O}(1)\) Converts a 'BitString' to a lazy 'ByteString'.
-- The 'BitString' is prepended with zeros if its length is not divisible by 8.
toByteString :: BitString -> ByteString
toByteString (BitString h 0 t) = t
toByteString (BitString h 8 t) = h `BL.cons` t
toByteString bs                = toByteString $ 0 ::: bs
{-# INLINE toByteString #-}

-- | \(\mathcal{O}(1)\) Converts a 'BitString' to a strict 'ByteString'.
-- The 'BitString' is prepended with zeros if its length is not divisible by 8.
toByteStringStrict :: BitString -> BS.ByteString
toByteStringStrict = BL.toStrict . toByteString
{-# INLINE toByteStringStrict #-}

-- | \(\mathcal{O}(1)\) Similar to 'toByteString', but also returns the
-- number of leading zeros.
toByteStringPadded :: BitString -> (Word8, ByteString)
toByteStringPadded Empty                = (0, BL.empty)
toByteStringPadded bs@(BitString _ l _) = (8 - l, toByteString bs)
{-# INLINE toByteStringPadded #-}

-- | \(\mathcal{O}(n)\) Converts a 'BitString' into a list of 'Bit's.
unpack :: BitString -> [Word8]
unpack Empty    = []
unpack (b:::bs) = b : unpack bs
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
drop 0 bs       = bs
drop _ Empty    = Empty
drop n (_:::bs) = drop (n - 1) bs
{-# INLINE drop #-}

-- | \(\mathcal{O}(n \cdot m)\) Drops \(n\) elements
-- from the end of the 'BitString'. Inefficient.
dropEnd :: Int64 -> BitString -> BitString
dropEnd 0 bs = bs
dropEnd n bs = case unsnoc bs of
    Nothing     -> empty
    Just (t, _) -> dropEnd (n - 1) t
{-# INLINE dropEnd #-}

-- | \(\mathcal{O}(n)\) Returns the prefix of 'BitString' of length \(n\)
-- or the 'BitString' itself if \(n\) is greater than the length of the
-- 'BitString'.
take :: Int64 -> BitString -> BitString
take _ Empty    = empty
take 0 bs       = empty
take n (b:::bs) = b ::: take (n - 1) bs
{-# INLINE take #-}

-- | \(\mathcal{O}(n \cdot m)\) Takes \(n\) elements
-- from the end of the 'BitString'. Inefficient.
takeEnd :: Int64 -> BitString -> BitString
takeEnd 0 bs = empty
takeEnd n bs = case unsnoc bs of
    Nothing     -> bs
    Just (i, l) -> takeEnd (n - 1) i `snoc` l
{-# INLINE takeEnd #-}

-- | \(\mathcal{O}(n)\), \(\Omega(n/c)\) Appends two 'BitString's.
-- In general not very efficient if length of the latter 'BitString' is not
-- divisible by 8.
append :: BitString -> BitString -> BitString
append Empty x = x
append x Empty = x
append (BitString h1 l1 t1) (BitString h2 8 t2) =
    BitString h1 l1 $ BL.append t1 $ h2 `BL.cons` t2
append a b = P.foldr cons b $ unpack a
{-# INLINE append #-}

-- | \(\mathcal{O}(n \cdot m)\) Concatenates a list of 'BitString's.
concat :: [BitString] -> BitString
concat = P.foldr append empty
{-# INLINE concat #-}

mapBytes :: (Word8 -> Word8) -> BitString -> BitString
mapBytes _ Empty             = empty
mapBytes f (BitString h l t) = BitString (f h) l $ BL.map f t
{-# INLINE mapBytes #-}

-- | \(\mathcal{O}(\min(n, m))\) Takes two 'BitString's and returns a list
-- of pairs of 'Bit's. If one 'BitString' is longer, its last elements are
-- discarded.
zip :: BitString -> BitString -> [(Word8, Word8)]
zip Empty _ = []
zip _ Empty = []
zip x y     = (head x, head y) : zip (tail x) (tail y)
{-# INLINE zip #-}

-- | \(\mathcal{O}(\min(n, m))\) Same as 'zip', but returns a list of pairs
-- of 'Bool's.
zipB :: BitString -> BitString -> [(Bool, Bool)]
zipB Empty _ = []
zipB _ Empty = []
zipB x y     = (headB x, headB y) : zipB (tail x) (tail y)
{-# INLINE zipB #-}

-- I don't see much use for this, but why not.
-- | \(\mathcal{O}(\min(n, m))\) Equivalent to 'Prelude' 'Prelude.zipWith'
-- for 'BitString's.
zipWith :: (Bool -> Bool -> a) -> BitString -> BitString -> [a]
zipWith _ Empty _ = []
zipWith _ _ Empty = []
zipWith f x y     = f (headB x) (headB y) : zipWith f (tail x) (tail y)
{-# INLINE zipWith #-}

-- | \(\mathcal{O}(\min(n, m))\) Generalized version of 'zipWith'.
-- Takes two 'BitString's and a binary function on 'Bool's and returns new
-- 'BitString'.
packZipWith :: (Bool -> Bool -> Bool) -> BitString -> BitString -> BitString
packZipWith _ Empty _ = empty
packZipWith _ _ Empty = empty
packZipWith f x y = f (headB x) (headB y)
    `consB` packZipWith f (tail x) (tail y)
{-# INLINE packZipWith #-}

packZipWithBytes :: (Word8 -> Word8 -> Word8)
                 -> BitString
                 -> BitString
                 -> BitString
packZipWithBytes _ Empty _ = empty
packZipWithBytes _ _ Empty = empty
packZipWithBytes f (BitString h1 l1 t1) (BitString h2 l2 t2) =
    BitString (f h1 h2) (min l1 l2) $ BL.packZipWith f t1 t2
{-# INLINE packZipWithBytes #-}

-- | \(\mathcal{O}(n)\) Equivalent to 'Prelude' 'Prelude.foldr'.
foldr :: (Bool -> a -> a) -> a -> BitString -> a
foldr _ x Empty = x
foldr f x bs    = f (headB bs) $ foldr f x (tail bs)
{-# INLINE foldr #-}

-- | \(\mathcal{O}(n)\) Like 'foldr', but strict in the accumulator.
foldr' :: (Bool -> a -> a) -> a -> BitString -> a
foldr' _ !x (BitString 0 0 BLI.Empty) = x
foldr' f !x bs                        = f (headB bs) $ foldr' f x (tail bs)
{-# INLINE foldr' #-}

-- | \(\mathcal{O}(n)\) Equivalent to 'Prelude' 'Prelude.foldl'.
foldl :: (a -> Bool -> a) -> a -> BitString -> a
foldl _ x Empty = x
foldl f x bs    = f (foldl f x (tail bs)) $ headB bs
{-# INLINE foldl #-}

-- | \(\mathcal{O}(n)\) Like 'foldl', but strict in the accumulator.
foldl' :: (a -> Bool -> a) -> a -> BitString -> a
foldl' _ !x (BitString 0 0 BLI.Empty) = x
foldl' f !x bs                        = f (foldl' f x (tail bs)) $ headB bs
{-# INLINE foldl' #-}

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

-- | Reads a file into a 'BitString'.
readFile :: FilePath -> IO BitString
readFile path = fromByteString <$> BL.readFile path
{-# INLINE readFile #-}

-- | Writes a 'BitString' to a file.
writeFile :: FilePath -> BitString -> IO ()
writeFile p = BL.writeFile p . toByteString
{-# INLINE writeFile #-}

-- | Appends a 'BitString' to a file.
appendFile :: FilePath -> BitString -> IO ()
appendFile p = BL.appendFile p . toByteString
{-# INLINE appendFile #-}

-- Legacy compatibility

{-# DEPRECATED bitString "Use 'fromByteStringStrict' instead" #-}
bitString :: BS.ByteString -> BitString
bitString = fromByteStringStrict
{-# INLINE bitString #-}

{-# DEPRECATED bitStringLazy "Use 'fromByteString' instead" #-}
bitStringLazy :: ByteString -> BitString
bitStringLazy = fromByteString
{-# INLINE bitStringLazy #-}

{-# DEPRECATED toList "Use 'unpackB' instead" #-}
toList :: BitString -> [Bool]
toList = unpackB
{-# INLINE toList #-}

{-# DEPRECATED to01List "Use 'unpack' instead" #-}
to01List :: BitString -> [Word8]
to01List = unpack
{-# INLINE to01List #-}

{-# DEPRECATED fromList "Use 'packB' instead" #-}
fromList :: [Bool] -> BitString
fromList = packB
{-# INLINE fromList #-}

{-# DEPRECATED from01List "Use 'pack' instead" #-}
from01List :: [Word8] -> BitString
from01List = pack
{-# INLINE from01List #-}

-- | \(\mathcal{O}(n)\) Creates a 'BitString' from a portion of
-- 'ByteString'. /Warning: No boundary checks are performed!/
unsafeBitString' :: Int64 -- ^ offset
                 -> Int64 -- ^ length
                 -> ByteString -- ^ source
                 -> BitString
unsafeBitString' o l bs = fromByteString $ BL.take l $ BL.drop 0 bs
{-# INLINE unsafeBitString' #-}

{-# DEPRECATED realizeBitStringStrict "Use 'toByteStringStrict' instead" #-}
realizeBitStringStrict :: BitString -> BS.ByteString
realizeBitStringStrict = toByteStringStrict
{-# INLINE realizeBitStringStrict #-}

{-# DEPRECATED realizeBitStringLazy "Use 'toByteString' instead" #-}
realizeBitStringLazy :: BitString -> ByteString
realizeBitStringLazy = toByteString
{-# INLINE realizeBitStringLazy #-}
