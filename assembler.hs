import Data.Map (Map) -- this imports the map consturctor only
import qualified Data.Map as Map -- this lets you reference all the things in Data.map with Map.<name>

import Numeric (showIntAtBase)
import Data.Char (intToDigit, digitToInt)
import Data.List (foldl', isPrefixOf)

import Text.Printf

preDefinedSymbols = [("R0", 0), ("R1", 1), ("R2", 2), ("R3", 3), ("R4", 4),
                     ("R5", 5), ("R6", 6), ("R7", 7), ("R8", 8), ("R9", 9),
                     ("R10", 10), ("R11", 11), ("R12", 12), ("R13", 13),
                     ("R14", 14), ("R15", 15), ("SCREEN", 16384), ("KBD", 24576),
                     ("SP", 0), ("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4)]
preDefinedMap = Map.fromList preDefinedSymbols

toBinaryStr :: Int -> String
toBinaryStr x = showIntAtBase 2 intToDigit x ""

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- pad binary strings with extra zeros
padded :: String -> String
padded x = printf "%016s" x

isLabel :: (Int, String) -> Bool
isLabel (idx, elm) = "(" `isPrefixOf` elm

testRun = do
  let file = "hack1.txt"
  contents <- readFile file

  -- TODO this is the first pass looking for labels
  -- you'll need to make a map out of the pairs left over
  -- after the filter and inc the index of each pair so
  -- it matches the line in the program that the label is pointing to
  print $ filter isLabel $ zip [1..] $ lines contents

  let file2 = "testy.asm"
  writeFile file2 "hahahahah"
