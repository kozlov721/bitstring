{-# OPTIONS_GHC -Wno-deprecations #-}
import Control.Monad   (void)
import Data.BitString  (BitString)
import Data.Bits
import Data.Int        (Int64)
import Data.List       (isInfixOf)
import Data.List.Extra (dropEnd, splitAtEnd, takeEnd)
import Data.Maybe      (fromJust, isNothing)
import Data.Word       (Word32, Word8)
import Test.HUnit
import Test.HUnit.Base (listAssert)
import Test.QuickCheck hiding ((.&.))

import qualified Data.Bifunctor       as Bi
import qualified Data.BitString       as BS
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
    [ "from-toByteString" ~: show bs
        ~: bs ~=? (uncurry BS.fromByteStringWithPadding . BS.toByteStringWithPadding) bs
    | bs <- [BS.fromNumber n | n <- [0..100]]
    ] ++
    [ "eq" ~: show n ~: BS.fromNumber n ~=? BS.fromNumber n
    | n <- [0..20]
    ] ++
    [ "null" ~: show bs ~: False ~=? BS.null bs
    | bs <- [BS.replicate n False | n <- [1..20]]
    ] ++
    [ "length" ~: show bits ~: fromIntegral (length bits) ~=? BS.length (BS.pack bits)
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
    [ "semigroup" ~: show a ++ " <> " ++ show b ++ " <> " ++ show c
        ~: (a <> b) <> c ~=? a <> b <> c
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
    [ "reverse" ~: show bs ~: bs ~=? (BS.reverse . BS.reverse) bs
    | bs <- [BS.fromNumber n | n <- [0..50]]
    ] ++
    [ "replicate" ~: show n ~: BS.packB (replicate (fromIntegral n) False)
        ~=? BS.replicate n False
    | n <- [0..100]
    ] ++
    [ "take" ~: show (take n b)
        ~: take n b ~=? BS.unpack (BS.take (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "drop" ~: show (drop n b)
        ~: drop n b ~=? BS.unpack (BS.drop (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "takeEnd" ~: show (takeEnd n b)
        ~: takeEnd n b ~=? BS.unpack (BS.takeEnd (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "dropEnd" ~: show (dropEnd n b)
        ~: dropEnd n b ~=? BS.unpack (BS.dropEnd (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "splitAt" ~: show (splitAt n b)
        ~: splitAt n b
        ~=? Bi.bimap BS.unpack BS.unpack
        (BS.splitAt (fromIntegral n) (BS.pack b))
    | b <- [toBinary n | n <- [0..50]]
    , n <- [0..10]
    ] ++
    [ "splitAtEnd" ~: show (splitAtEnd n b)
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


newtype Size     = Size     Int64  deriving Show
newtype BoolList = BoolList [Bool] deriving Show

newtype SearchFor = SearchFor BitString deriving Show

instance Arbitrary Size where
  arbitrary = Size . (fromIntegral :: Int -> Int64) <$> choose (0,64) -- 192)

instance Arbitrary BoolList where
  arbitrary = do
    Size k <- arbitrary
    BoolList <$> vector (fromIntegral k)

-- with 48 bits, it's unlikely that there are other random appearances
instance Arbitrary SearchFor where
  arbitrary = do
    b <- arbitrary
    let l = BS.length b
    if l >= 48 && l < 96
      then return (SearchFor b)
      else arbitrary

instance Arbitrary BitString where
  arbitrary = do
    k <- choose (0,7)
    BS.pack <$> vector k

-- (some) legacy tests

runAllTest :: IO ()
runAllTest = do
  let mytest (text,prop) = do
        print text
        quickCheck prop

  mytest ("fromToList"    , prop_fromToList )
  mytest ("toFromList"    , prop_toFromList )
  mytest ("append"       , prop_append     )
  mytest ("drop"         , prop_drop       )
  mytest ("take"         , prop_take       )
  mytest ("realizeLen"   , prop_realizeLen   )

prop_fromToList :: BitString -> Bool
prop_fromToList bits = BS.fromList (BS.toList bits) == bits

prop_toFromList :: BoolList -> Bool
prop_toFromList (BoolList list) = BS.toList (BS.fromList list) == list

prop_append :: [BitString] -> Bool
prop_append xs = BS.toList (BS.concat xs) == concatMap BS.toList xs

prop_drop :: Size -> BitString -> Bool
prop_drop (Size k) xs = BS.toList (BS.drop k xs) == List.drop (fromIntegral k) (BS.toList xs)

prop_take :: Size -> BitString -> Bool
prop_take (Size k) xs = BS.toList (BS.take k xs) == List.take (fromIntegral k) (BS.toList xs)

prop_realizeLen :: BitString -> Bool
prop_realizeLen bits =
    let n = BS.length bits
    in  (n + 7) `div` 8 == fromIntegral (BSS.length $ BS.realizeBitStringStrict bits)

toBinary :: Word32 -> [Word8]
toBinary 0 = []
toBinary n = fromIntegral (n `mod` 2) : toBinary (n `div` 2)

toBinaryPad :: Word32 -> [Word8]
toBinaryPad n = replicate (32 - length bits) 0 ++ bits
  where
    bits = reverse $ toBinary n

main :: IO ()
main = do
    runTestTT $ test $ simpleTests ++ bitsTests
    runAllTest

