{-# LANGUAGE MultiWayIf #-}
import qualified Data.Map as M -- this lets you reference all the things in Data.map with M.<name>
import Numeric (showIntAtBase)
import Data.Char (intToDigit, digitToInt, isSpace, isControl)
import Data.List (foldl', isPrefixOf, dropWhileEnd, nub)
import Data.Maybe
import Text.ParserCombinators.ReadP
import Text.Printf
import Text.Read
import Control.Applicative hiding (optional)
import System.IO
import qualified Control.Monad.State as S

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
zeroPad = printf "%016s"

labelP :: ReadP String
labelP = do
  satisfy (== '(')
  label <- many1 (satisfy (\char -> (not $ isParen char)))
  satisfy (== ')')
  return label

extractLabel :: String -> String
-- output of readP_to_S when parse successful: [("label123","")]
extractLabel label = fst $ head $ readP_to_S labelP label

isLabel :: String -> Bool
isLabel = isPrefixOf "("

isComment :: String -> Bool
isComment = isPrefixOf "//"

isAIns :: String -> Bool
isAIns = isPrefixOf "@"

isParen :: Char -> Bool
isParen char = any (char ==) "()"

isUserSymbol :: (M.Map String Int) -> String -> Bool
isUserSymbol symbols line
               | isAIns line = not $ M.member cleanline symbols
               | otherwise = False
             where cleanline = dropWhile ('@' ==) line

{- C Intruction Parsing
   dest = comp ; jump (both dest and jump are optional)
   binary: 111 a c1 c2 c3 c4 c5 c6 d1 d2 d3 j1 j2 j3
   a value: determined by command type
-}
data CIns = CIns {dest :: Maybe String,
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

cInstructionP :: ReadP CIns
cInstructionP = do
  dest <- option Nothing (fmap Just destP)
  option Nothing $ (fmap Just $ string "=")
  comp <- compP
  option Nothing $ (fmap Just $ string ";")
  jump <- option Nothing (fmap Just jumpP)
  return (CIns dest comp jump)

destIns :: (M.Map String String)
destIns = M.fromList [("M", "001"), ("D", "010"), ("MD", "011"), ("A", "100"),
                      ("AM", "101"), ("AD", "110"), ("AMD", "111")]

type CompInstruction = (Cbits, Abit)
type Cbits = String
type Abit = String

compIns :: (M.Map String CompInstruction)
compIns = M.fromList [("0", ("101010", "0")), ("1", ("111111", "0")), ("-1", ("111010", "0")),
                      ("D", ("001100", "0")), ("A", ("110000", "0")), ("!D", ("001101", "0")),
                      ("!A", ("110001", "0")), ("-D", ("001111", "0")), ("-A", ("110011", "0")),
                      ("D+1", ("011111", "0")), ("A+1", ("110111", "0")), ("D-1", ("001110", "0")),
                      ("A-1", ("110010", "0")), ("D+A", ("000010", "0")), ("D-A", ("010011", "0")),
                      ("A-D", ("000111", "0")), ("D&A", ("000000", "0")), ("D|A", ("010101", "0")),
                      ("M", ("110000", "1")), ("!M", ("110001", "1")), ("-M", ("110011", "1")),
                      ("M+1", ("110111", "1")), ("M-1", ("110010", "1")), ("D+M", ("000010", "1")),
                      ("D-M", ("010011", "1")), ("M-D", ("000111", "1")), ("D&M", ("000000", "1")),
                      ("D|M", ("010101", "1"))]

jumpIns :: (M.Map String String)
jumpIns = M.fromList [("JGT", "001"), ("JEQ", "010"), ("JGE", "011"), ("JLT", "100"), ("JNE", "101"),
                      ("JLE", "110"), ("JMP", "111")]

lookupIns :: (M.Map String String) -> Maybe String -> String
lookupIns _ Nothing = "000"
lookupIns m (Just k) = m M.! k

procuceCBits :: CIns -> String
procuceCBits (CIns { dest = d, comp = c, jump = j }) =
   -- Ins format: 111 a c1 c2 c3 c4 c5 c6 d1 d2 d3 j1 j2 j3
  "111" ++ abit ++ cbits ++ dbits ++ jbits
  where (cbits, abit) = compIns M.! c
        dbits = lookupIns destIns d
        jbits = lookupIns jumpIns j

translateC :: String -> Maybe String
translateC line = case parse of
  [] -> Nothing
  (x:xs) -> Just $ procuceCBits $ fst x
  -- most complete parse is at end of readP_to_S output, see reverse below
  where parse = (reverse $ readP_to_S cInstructionP line)

translateLine :: (M.Map String Int) -> String -> Maybe String
translateLine symbols line
                | isAIns line = Just (produceOutput $ symbols M.! cleanline)
                | otherwise = translateC line
              where cleanline = dropWhile ('@' ==) line
                    produceOutput = zeroPad . toBinaryStr

handleLiteralInt :: String -> Maybe (String, Int)
-- account for the case where an A instruction is an integer value
-- ex. @32 should produce a ("32", 32) entry in the lookup map
handleLiteralInt sym =
  case readMaybe sym :: Maybe Int of
    Just s -> Just (sym, s)
    Nothing -> Nothing

type Lookup = M.Map String Int
type Labels = Lookup
type Symbols = Lookup
type LineNum = Int
type SymbolLookupState = (Labels, Symbols, LineNum)

insert' :: (String, Int) -> M.Map String Int -> M.Map String Int
insert' (line, idx) m = M.insert line idx m

extractLabelsLitInts :: String -> S.State SymbolLookupState SymbolLookupState
extractLabelsLitInts line = do
  (labels, symbols, lineNum) <- S.get
  if
    | isLabel line -> S.put (insert' (extractLabel line, lineNum) labels, symbols, lineNum)
    | isJust $ litIntParse -> S.put (labels, insert' (fromJust litIntParse) symbols, lineNum + 1)
    | otherwise -> S.put (labels, symbols, lineNum + 1)
  return (labels, symbols, lineNum)
  where litIntParse = handleLiteralInt $ dropWhile ('@' ==) line

buildLabelsLitInts :: [String] -> M.Map String Int
buildLabelsLitInts lines = M.union labels symbols
  where (labels, symbols, _ ) = S.execState (mapM extractLabelsLitInts lines) (M.empty, M.empty, 0)

buildSymbolLookup :: [String] -> M.Map String Int
buildSymbolLookup lines = M.union labelsAndSymbols userSymbols
  where labelsAndSymbols = M.union builtInSymbols $ buildLabelsLitInts lines
        userSymbols = M.fromList $
                      (\xs -> zip xs [16..]) $ -- start at memory location 16 and assign slots to user symbols
                      nub $ -- remove dups, nub is quadratic but input should be small and this is for fun :)
                      map (\line -> dropWhile ('@' == ) line) $
                      filter (isUserSymbol labelsAndSymbols) lines

prepLines :: String -> [String]
prepLines contents = cleanLines
  where cleanLines = filter isNotCommentOrEmpty $
                     map (dropWhileEnd isControlOrSpace) $
                     map (dropWhile isSpace) $
                     lines contents
        isControlOrSpace c = ((isControl c) || (isSpace c))
        isNotCommentOrEmpty l = not $ (isComment l || null l)

testRun = do
  let file = "Pong.asm"
  contents <- readFile file

  let cleanedLines = prepLines contents
  let finalSymbolLookup = buildSymbolLookup cleanedLines

  let output = unlines $ catMaybes $ map (translateLine finalSymbolLookup) cleanedLines

  outy <- openFile "Pong.hack" WriteMode
  hPutStrLn outy output
  hClose outy
