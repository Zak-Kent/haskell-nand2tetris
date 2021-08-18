import qualified Data.Map as M -- this lets you reference all the things in Data.map with M.<name>
import Numeric (showIntAtBase)
import Data.Char (intToDigit, digitToInt, isSpace, isControl)
import Data.List (foldl', isPrefixOf, dropWhileEnd, nub)
import Text.ParserCombinators.ReadP
import Text.Printf
import Control.Applicative hiding (optional)

preDefinedSymbols = [("R0", 0), ("R1", 1), ("R2", 2), ("R3", 3), ("R4", 4),
                     ("R5", 5), ("R6", 6), ("R7", 7), ("R8", 8), ("R9", 9),
                     ("R10", 10), ("R11", 11), ("R12", 12), ("R13", 13),
                     ("R14", 14), ("R15", 15), ("SCREEN", 16384), ("KBD", 24576),
                     ("SP", 0), ("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4)]
builtInSymbols = M.fromList preDefinedSymbols

toBinaryStr :: Int -> String
toBinaryStr x = showIntAtBase 2 intToDigit x ""

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- zeroPad binary strings with extra zeros
zeroPad :: String -> String
zeroPad x = printf "%016s" x

isLabel :: (String, Int) -> Bool
isLabel (line, idx) = "(" `isPrefixOf` line

isComment :: String -> Bool
isComment line = "//" `isPrefixOf` line

isAIns :: String -> Bool
isAIns line = "@" `isPrefixOf` line

isParen :: Char -> Bool
isParen char = any (char ==) "()"

isUserSymbol :: (M.Map String Int) -> (String, Int) -> Bool
isUserSymbol symbols (line, idx)
               | isAIns line = not $ M.member cleanline symbols
               | otherwise = False
             where cleanline = dropWhile ('@' ==) line

labelP :: ReadP String
labelP = do
  satisfy (== '(')
  label <- many1 (satisfy (\char -> (not $ isParen char)))
  satisfy (== ')')
  return label

extractLabel :: String -> String
-- output of readP_to_S when parse successful: [("label123","")]
extractLabel label = fst $ head $ readP_to_S labelP label

prepLines :: String -> [(String, Int)]
prepLines contents = zip cleanLines [1..]
  where cleanLines = map (dropWhileEnd isControlOrSpace) $
                     map (dropWhile isSpace) $
                     lines contents
        isControlOrSpace c = ((isControl c) || (isSpace c))

{- C Intruction Parsing
   dest = comp ; jump (both dest and jump are optional)
   binary: 111 a c1 c2 c3 c4 c5 c6 d1 d2 d3 j1 j2 j3
   a value: determined by command type
-}

data CInstruction = CInstruction
                    {dest :: Maybe String,
                     comp :: String,
                     jump :: Maybe String
                    } deriving Show

destP :: ReadP String
destP = string "M" <|> string "D" <|> string "MD" <|> string "A" <|>
        string "AM" <|> string "AD" <|> string "AMD"

compP :: ReadP String
compP = string "0" <|> string "1" <|> string "-1" <|> string "D" <|>
        string "A" <|> string "!D" <|> string "!A" <|> string "-D" <|>
        string "-A" <|> string "D+1" <|> string "A+1" <|> string "D-1" <|>
        string "A-1" <|> string "D+A" <|> string "D-A" <|> string "A-D" <|>
        string "D&A" <|> string "D|A" <|> string "M" <|> string "!M" <|>
        string "-M" <|> string "M+1" <|> string "M-1" <|> string "D+M" <|>
        string "D-M" <|> string "M-D" <|> string "D&M" <|> string "D|M"

jumpP :: ReadP String
jumpP = string "JGT" <|> string "JEQ" <|> string "JGE" <|> string "JLT" <|>
        string "JNE" <|> string "JLE" <|> string "JMP"

cInstructionP :: ReadP CInstruction
cInstructionP = do
  dest <- option Nothing (fmap Just destP)
  option Nothing $ (fmap Just $ string "=")
  comp <- compP
  option Nothing $ (fmap Just $ string ";")
  jump <- option Nothing (fmap Just jumpP)
  return (CInstruction dest comp jump)

translateC :: String -> Maybe String
translateC line = Just "C instruction"

translateLine :: (M.Map String Int) -> (String, Int) -> Maybe String
translateLine symbols (line, idx)
                | isAIns line = Just (produceOutput $ symbols M.! cleanline)
                | isComment line = Nothing
                | otherwise = translateC line
              where cleanline = dropWhile ('@' ==) line
                    produceOutput = zeroPad . toBinaryStr

testRun = do
  let file = "Max.asm"
  contents <- readFile file
  let contentWithLineNums = prepLines contents

  let labels = M.fromList $
               -- the label refers to the following line in the asm script
               -- which is why there is a +1 on the line number
               map (\label -> (extractLabel (fst label), (+ 1) (snd label))) $
               filter isLabel contentWithLineNums

  -- combine labels and built in symbols
  let labelsAndSymbols = M.union labels builtInSymbols

  -- do another pass to find any user symbols which aren't labels
  -- TODO: could combine label and user symbol pass into one when time allows
  let userSymbols =
        M.fromList $
        (\xs -> zip xs [16..]) $ -- start at memory location 16 and assign slots to user symbols
        nub $ -- remove dups, nub is quadratic but input should be small and this is for fun :)
        map (\line -> dropWhile ('@' == ) (fst line)) $
        filter (isUserSymbol labelsAndSymbols) contentWithLineNums

  let finalSymbolLookup = M.union labelsAndSymbols userSymbols

  let output = map (translateLine finalSymbolLookup) contentWithLineNums
  print output

{-
1. you need to map over all the lines in contents and first decide if line is A vs. C instruction
2. need to make a lookup map for C instruction translations, if C then translate
3. if A instruction:
    - check label map for match:
        - if match then replace instuction with converted binary string value for label
        - if no match move to next check

    - check predefined symbol for match:
        - if match then replace instuction with converted binary string value for symbol
        - if no match add symbol and assign memory location value that's next in line starting at 16
        - ^^ will need to check the map or keep track of how many memory slots have been handed out
        - write binary string value for newly added symbol

?? how does this tell the difference between the memory location and a location in the script?
^^ I think the next instruciton probably figures that out the A register can be set to 17
which could match either line 17 in the script or register 17 and the PC probably jumps in one
case and not the other
-}
