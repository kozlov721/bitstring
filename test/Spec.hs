import Control.Monad  (void)
import Data.BitString (BitString)
import Data.Maybe     (fromJust)
import Data.Word      (Word8)
import Test.HUnit

import qualified Data.BitString       as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List


tests = test $
    [ "cons-uncons" ~: show bits ~: bits ~=? bitsCycle bits
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "bool-cons-uncons" ~: show bits ~: bits ~=? bitsCycleB bits
    | bits <- [map (/=0) $ toBinary n | n <- [0..100]]
    ] ++
    [ "unpack-pack" ~: show bits ~: bits ~=? (BS.unpack . BS.pack) bits
    | bits <- [toBinary n | n <- [0..100]]
    ]

bitsCycle :: [Word8] -> [Word8]
bitsCycle = BS.unpack . foldr BS.cons BS.empty

bitsCycleB :: [Bool] -> [Bool]
bitsCycleB = map (==1) . BS.unpack . foldr BS.consB BS.empty

toBinary :: Word8 -> [Word8]
toBinary 0 = []
toBinary n = (n `mod` 2) : toBinary (n `div` 2)

-- Legacy Tests

main :: IO ()
main =  void $ runTestTT tests

