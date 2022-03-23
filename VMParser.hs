module VMParser where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)
import Data.Char (isSpace)
import Data.Maybe

{- MemSegment data type
   The VM has 8 memory segements, 4 of which have static base addresses in RAM
   and 4 are pointer based. The MemSegment type is used to capture the name,
   segment type (Point/Fixed), and base address of each segement. -}
data SegType = Fixed | Point deriving (Show)
type BaseAddr = Integer
data SegName = Local | Argument | This | That | Constant
             | Static | Temp | Pointer deriving (Show)
data MemSegment = MemSegment { segName :: SegName,
                               baseAddr :: Maybe BaseAddr,
                               segType :: SegType
                               } deriving (Show)

{- Command data type
   Currently supported VM commands:
     - push <memSegment> <index>
     - pop <memSegment> <index>
     - arithmetic: add, sub, and, or, eq, gt, lt, not, neg
     - labeled commands: goto <label>, if-goto <label>, label <label>
                         call <label>, function <label>
     - return
-}
data Command = Arithmetic String | Push | Pop |Goto String | IfGoto String
               | Label String | Call String | Function String | Return
             deriving (Show)
type Index = Integer
data Line = Line { command :: Command,
                   memSegment :: Maybe MemSegment,
                   index :: Maybe Index
                 } deriving (Show)

-- VM command parsing --

pushP :: ReadP Command
pushP = do
  _ <- string "push"
  return Push

popP :: ReadP Command
popP = do
  _ <- string "pop"
  return Pop

returnP :: ReadP Line
returnP = do
  _ <- string "return"
  return (Line Return Nothing Nothing)

toLabeledCommand :: String -> String -> Command
toLabeledCommand cmd label = case cmd of
                               "goto" -> Goto label
                               "if-goto" -> IfGoto label
                               "label" -> Label label
                               "call" -> Call label
                               "function" -> Function label

labeledCommandP :: ReadP Line
labeledCommandP = do
  cmd <- string "goto" <|> string "if-goto" <|> string "label"
         <|> string "call" <|> string "function"
  _ <- satisfy isSpace
  label <- munch1 (\c -> not $ isSpace c)
  return $ (Line (toLabeledCommand cmd label) Nothing Nothing)

arithmeticP :: ReadP Line
arithmeticP = do
  arith <- string "add" <|> string "sub" <|> string "neg" <|> string "eq" <|>
           string "gt" <|> string "lt" <|> string "and" <|> string "or" <|>
           string "not"
  return (Line (Arithmetic arith) Nothing Nothing)

toMemSegment :: String -> MemSegment
toMemSegment seg = case seg of
                     "local" -> (MemSegment Local (Just 1) Point)
                     "argument" -> (MemSegment Argument (Just 2) Point)
                     "this" -> (MemSegment This (Just 3) Point)
                     "that" -> (MemSegment That (Just 4) Point)
                     "constant" -> (MemSegment Constant Nothing Fixed)
                     "static" -> (MemSegment Static (Just 16) Fixed)
                     "temp" -> (MemSegment Temp (Just 5) Fixed)
                     "pointer" -> (MemSegment Pointer (Just 3) Fixed)

memSegmentP :: ReadP MemSegment
memSegmentP = do
  seg <- string "local" <|> string "argument" <|> string "this" <|>
         string "that" <|> string "constant" <|> string "static" <|>
         string "temp" <|> string "pointer"
  return $ toMemSegment seg

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

indexP :: ReadP Index
indexP = fmap read $ munch1 isDigit

pushPopP :: ReadP Line
pushPopP = do
  cmd <- popP <|> pushP
  _ <- satisfy isSpace
  memSeg <- fmap Just memSegmentP
  _ <- satisfy isSpace
  idx <- fmap Just indexP
  return (Line cmd memSeg idx)

lineP :: ReadP Line
lineP = do
  line <- arithmeticP <|> pushPopP <|> labeledCommandP <|> returnP
  return line

checkParse :: [(a, b)] -> Maybe a
checkParse [] = Nothing
checkParse (x:_) = Just (fst x)

parseLines :: [String] -> [Line]
-- type of 'map readP_toS lines' :: [[(Line, String)]]
parseLines l = catMaybes $ map checkParse $ map (readP_to_S lineP) l
