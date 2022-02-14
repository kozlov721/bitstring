import Control.Monad  (void)
import Data.BitString (BitString)
import Data.Bits
import Data.Maybe     (fromJust)
import Data.Word      (Word32, Word8)
import Test.HUnit
import Test.HUnit.Base (listAssert)

import qualified Data.BitString       as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List            as List


simpleTests =
    [ "cons-uncons" ~: show bits ~: bits ~=? bitsCycle bits
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "bool-cons-uncons" ~: show bits ~: bits ~=? bitsCycleB bits
    | bits <- [map (/=0) $ toBinary n | n <- [0..100]]
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
    [ "eq" ~: show n ~: BS.fromNumber n ~=? BS.fromNumber n
    | n <- [0..20]
    ] ++
    [ "length" ~: show bits ~: fromIntegral (length bits) ~=? BS.length (BS.pack bits)
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "append" ~: show (x ++ y)
        ~: x ++ y ~=? BS.unpack (BS.append (BS.pack x) (BS.pack y))
    | x <- [toBinary n | n <- [0..40]]
    , y <- [toBinary n | n <- [0..40]]
    ] ++
    [ "read-show" ~: show bits
        ~: let bs = BS.pack bits in bs ~=? (read . show) bs
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "semigroup" ~: show a ++ " <> " ++ show b ++ " <> " ++ show c
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
    ]

bitsCycle :: [Word8] -> [Word8]
bitsCycle = BS.unpack . foldr BS.cons BS.empty

bitsCycleB :: [Bool] -> [Bool]
bitsCycleB = map (/=0) . BS.unpack . foldr BS.consB BS.empty

toBinary :: Word32 -> [Word8]
toBinary 0 = []
toBinary n = fromIntegral (n `mod` 2) : toBinary (n `div` 2)

toBinaryPad :: Word32 -> [Word8]
toBinaryPad n = replicate (32 - length bits) 0 ++ bits
  where
    bits = reverse $ toBinary n

main :: IO ()
main =  void $ runTestTT $ test $ simpleTests ++ bitsTests

