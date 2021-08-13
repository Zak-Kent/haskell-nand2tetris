import Data.Map (Map) -- this imports the map consturctor only
import qualified Data.Map as Map -- this lets you reference all the things in Data.map with Map.<name>

import Numeric (showIntAtBase)
import Data.Char (intToDigit, digitToInt)
import Data.List (foldl', isPrefixOf)
import Text.ParserCombinators.ReadP


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

-- zeroPad binary strings with extra zeros
zeroPad :: String -> String
zeroPad x = printf "%016s" x

isLabel :: (String, Int) -> Bool
isLabel (line, idx) = "(" `isPrefixOf` line

isComment :: (String, Int) -> Bool
isComment (line, idx) = "//" `isPrefixOf` line

isParen :: Char -> Bool
isParen char = any (char ==) "()"

labelP :: ReadP String
labelP = do
  satisfy (== '(')
  label <- many1 (satisfy (\char -> (not $ isParen char)))
  satisfy (== ')')
  return label

extractLabel :: String -> String
-- output of readP_to_S when parse successful: [("label123","")]
extractLabel label =  fst $ head $ readP_to_S labelP label


testRun = do
  let file = "Max.asm"
  contents <- readFile file
  let labels = Map.fromList $
               -- the label refers to the following line in the asm script
               map (\label -> (extractLabel (fst label), (+ 1) (snd label))) $
               filter isLabel $
               zip (lines contents) [1..]
  print labels
