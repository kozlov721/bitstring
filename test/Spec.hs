{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE CPP #-}

import Control.Monad (void)

#ifdef BIGENDIAN
import Data.BitString.BigEndian (BitString)
#else
import Data.BitString           (BitString)
#endif

import Data.Bits
import Data.Int        (Int64)
import Data.List       (isInfixOf, dropWhileEnd)
import Data.List.Extra (dropEnd, splitAtEnd, takeEnd)
import Data.Maybe      (fromJust, isNothing)
import Data.Word
import Test.HUnit
import Test.HUnit.Base (listAssert)
import Test.QuickCheck hiding ((.&.))

import qualified Data.Bifunctor as Bi

#ifdef BIGENDIAN
import qualified Data.BitString.BigEndian as BS
#else
import qualified Data.BitString           as BS
#endif

import qualified Data.ByteString      as BSS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List            as List


simpleTests =
    [ "inner-consistency" ~: show bs ~: bs
        ~=? BS.fromByteString (BS.toByteString (BS.tail (1 `BS.cons` bs)))
    | bs <- [BS.pack $ toBinaryPad n | n <- [0..100]]
    ] ++
    [ "unpack-pack" ~: show bits ~: bits ~=? (BS.unpack . BS.pack) bits
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "bool-unpack-pack" ~: show bits ~: bits ~=? (BS.unpackB . BS.packB) bits
    | bits <- [map (/=0) $ toBinary n | n <- [0..100]]
    ] ++
    [ "from-toNumber" ~: show n ~: n ~=? (BS.toNumber . BS.fromNumber) n
    | n <- [0..100]
    ] ++
    [ "from-toNumberPadded" ~: show n
        ~: n ~=? (BS.toNumber . BS.fromNumberPadded) n
    | n <- [0..100] :: [Word16]
    ] ++
    [ "fromNumberPadded-length" ~: show n
        ~: BS.length (BS.fromNumberPadded n) ~=? 16
    | n <- [0..100] :: [Word16]
    ] ++
    [ "read-show" ~: show bs ~: bs ~=? (read . show) bs
    | bs <- [BS.fromNumber n | n <- [0..100]]
    ] ++
    [ "from-toByteString" ~: show bs
        ~: bs ~=? (uncurry BS.fromByteStringPadded . BS.toByteStringPadded) bs
    | bs <- [BS.fromNumber n | n <- [0..100]]
    ] ++
    [ "eq" ~: show n ~: BS.fromNumber n ~=? BS.fromNumber n
    | n <- [0..20]
    ] ++
    [ "neq" ~: show x ++ " /= " ++ show y
        -- why is there no ~/=?
        ~: BS.fromNumber x /= BS.fromNumber y ~=? True
    | x <- [0..10]
    , y <- [11..20]
    ] ++
    [ "null" ~: show bs ~: BS.null bs ~=? False
    | bs <- [BS.replicate n False | n <- [1..20]]
    ] ++
    [ "length" ~: show bits
        ~: fromIntegral (length bits) ~=? BS.length (BS.pack bits)
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "append" ~: show (x ++ y)
        ~: x ++ y ~=? BS.unpack (BS.append (BS.pack x) (BS.pack y))
    | x <- [toBinary n | n <- [0..20]]
    , y <- [toBinary n | n <- [0..20]]
    ] ++
    [ "concat" ~: show (x ++ y ++ z) ~: BS.pack (x ++ y ++ z)
        ~=? BS.concat (BS.pack <$> [x, y, z])
    | x <- [toBinary n | n <- [0..10]]
    , y <- [toBinary n | n <- [0..10]]
    , z <- [toBinary n | n <- [0..10]]
    ] ++
    [ "read-show" ~: show bits
        ~: let bs = BS.pack bits in bs ~=? (read . show) bs
    | bits <- [toBinary n | n <- [0..50]]
    ] ++
    [ "append" ~: show a ++ " <> " ++ show b
        ~: (BS.pack a <> BS.pack b) ~=? BS.pack (a ++ b)
    | a <- [toBinary n | n <- [0..20]]
    , b <- [toBinary n | n <- [20..40]]
    ] ++
    [ "length" ~: show bits
        ~: fromIntegral (length bits) ~=? BS.length (BS.pack bits)
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "semigroup-asoc" ~: show a ++ " <> " ++ show b ++ " <> " ++ show c
        ~: (a <> b) <> c ~=? a <> (b <> c)
    | a <- [BS.fromNumber n | n <- [0..10]]
    , b <- [BS.fromNumber n | n <- [0..10]]
    , c <- [BS.fromNumber n | n <- [0..10]]
    ] ++
    [ "monoid-right" ~: show bs ~: bs <> mempty ~=? bs
    | bs <- [BS.fromNumber n | n <- [0..10]]
    ] ++
    [ "monoid-left" ~: show bs ~: mempty <> bs ~=? bs
    | bs <- [BS.fromNumber n | n <- [0..10]]
    ] ++
    [ "reverse" ~: show bits
        ~: (BS.pack . reverse) bits ~=? (BS.reverse . BS.pack) bits
    | bits <- [toBinary n | n <- [0..200]]
    ] ++
    [ "replicate" ~: show n ~: BS.packB (replicate (fromIntegral n) False)
        ~=? BS.replicate n False
    | n <- [0..100]
    ] ++
    [ "take" ~: show n ++ " " ++ show b
        ~: take n b ~=? BS.unpack (BS.take (fromIntegral n) (BS.pack b))
    | b <- [toBinaryPad n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "drop" ~: show n ++ " " ++ show b
        ~: drop n b ~=? BS.unpack (BS.drop (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "takeEnd" ~: show n ++ " " ++ show b
        ~: takeEnd n b ~=? BS.unpack (BS.takeEnd (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "dropEnd" ~: show n ++ " " ++ show b
        ~: dropEnd n b ~=? BS.unpack (BS.dropEnd (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++ concat [
        [ "strip" ~: show bits ++ " " ++ show b
            ~: BS.pack (dropWhile (==b) bits) ~=? (f . BS.pack) bits
        | bits <- [toBinary n | n <- [0..200]]
        ] ++
        [ "stripEnd" ~: show bits ++ " " ++ show b
            ~: BS.pack (dropWhileEnd (==b) bits) ~=? (g . BS.pack) bits
        | bits <- [toBinary n | n <- [0..200]]
        ] | (f, g, b) <- [ (BS.stripOnes,  BS.stripOnesEnd,  1)
                         , (BS.stripZeros, BS.stripZerosEnd, 0)
                         ]
    ] ++
    [ "splitAt" ~: show n ++ " " ++ show b
        ~: splitAt n b
        ~=? Bi.bimap BS.unpack BS.unpack
        (BS.splitAt (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "splitAtEnd" ~: show n ++ " " ++ show b
        ~: splitAtEnd n b
        ~=? Bi.bimap BS.unpack BS.unpack
        (BS.splitAtEnd (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    ["findSubstring" ~: show a ++ " : " ++ show b ++ " : " ++ show c
        ~: let x = BS.concat [a, b, c]
           in  Just (BS.length a) ~=? BS.findSubstring b x
    | a <- [BS.replicate n False | n <- [0..10]]
    , b <- [BS.fromNumber n | n <- [10,9..1]]
    , c <- [BS.fromNumber n | n <- [0..10]]
    ]

bitsTests = concat
    [
        [str ~: (show x ++ str ++ show y)
            ~: zipWith fint x y ~=? BS.unpack (BS.pack x `fbits` BS.pack y)
        | x <- [toBinaryPad n | n <- [70..100]]
        , y <- [toBinaryPad n | n <- [70..100]]
        ]
    | (fint, fbits, str) <- [ ((.&.), (.&.), " .&. ")
                            , ((.|.), (.|.), " .|. ")
                            , (xor, xor, " xor ")]
    ] ++
    [ "complement" ~: show bits
        ~: map not bits ~=? BS.unpackB (complement (BS.packB bits))
    | bits <- [map (/=0) (toBinaryPad n) | n <- [0..100]]
    ] ++
    [ "shift" ~: show b ++ " `shift` " ++ show n
        ~:  toBinaryPad (b `shift` n)
        ~=? BS.unpack (BS.pack (toBinaryPad b) `shift` n)
    | b <- [0..50] :: [Word32]
    , n <- [-10..10] :: [Int]
    ] ++
    [ "rotate" ~: show b ++ " `rotate` " ++ show n
        ~:  toBinaryPad (b `rotate` n)
        ~=? BS.unpack (BS.pack (toBinaryPad b) `rotate` n)
    | b <- [0..50] :: [Word32]
    , n <- [-10..10] :: [Int]
    ] ++
    [ "testBit-true" ~: show n ~: testBit (bit n :: BitString) n ~=? True
    | n <- [0..100] :: [Int]
    ] ++
    [ "testBit-false" ~: show n ++ " " ++ show m
        ~: testBit (bit n :: BitString) m ~=? False
    | n <- [0..50] :: [Int]
    , m <- [n+1..50] :: [Int]
    ] ++
    [ "popCount" ~: show bits
        ~: fromIntegral (sum bits) ~=? popCount (BS.pack bits)
    | bits <- [toBinary n | n <- [0..100]]
    ]

toBinary :: Word32 -> [Word8]
toBinary 0 = []
toBinary n = fromIntegral (n `mod` 2) : toBinary (n `div` 2)

toBinaryPad :: Word32 -> [Word8]
toBinaryPad n = replicate (32 - length bits) 0 ++ bits
  where
    bits = reverse $ toBinary n

main :: IO ()
main = void . runTestTT . test $ simpleTests ++ bitsTests
