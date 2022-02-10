import Control.Monad  (void)
import Data.BitString (BitString)
import Data.Maybe     (fromJust)
import Data.Word      (Word8)
import Test.HUnit

import qualified Data.BitString       as BS
import qualified Data.ByteString.Lazy as BL


tests = test $
    [ "cons-uncons" ~: show bits ~: bits ~=? bitsCycle bits
    | bits <- [toBinary n | n <- [0..100]]
    ] ++
    [ "bool-cons-uncons" ~: show bits ~: bits ~=? bitsCycleB bits
    | bits <- [map (==1) $ toBinary n | n <- [0..100]]
    ] ++
    [ "toFromBits" ~: show bits ~: bits ~=? toBits (fromBits bits)
    | bits <- [map (==1) $ toBinary n | n <- [0..100]]
    ]

bitsCycle :: [Word8] -> [Word8]
bitsCycle = BS.toBits . foldr BS.cons BS.empty

bitsCycleB :: [Bool] -> [Bool]
bitsCycleB = map (==1) . BS.toBits . foldr BS.consB BS.empty

toBinary :: Word8 -> [Word8]
toBinary 0 = []
toBinary n = (n `mod` 2) : toBinary (n `div` 2)

main :: IO ()
main =  void $ runTestTT tests

