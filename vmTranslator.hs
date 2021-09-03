import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)
import Data.Char (isSpace)

data Command = Arithmetic String | Push | Pop
             deriving (Show)
data MemSegment = Local | Argument | This | That | Constant | Static | Temp | Pointer
                deriving (Show)
type Index = Integer

data Line = Line { command :: Command,
                   memSegment :: Maybe MemSegment,
                   index :: Maybe Index
                 } deriving (Show)

pushP :: ReadP Command
pushP = do
  string "push"
  return Push

popP :: ReadP Command
popP = do
  string "pop"
  return Pop

arithmeticP :: ReadP Command
arithmeticP = do
  arith <- string "add" <|> string "sub" <|> string "neg" <|> string "eq" <|>
           string "gt" <|> string "lt" <|> string "and" <|> string "or" <|>
           string "not"
  return $ Arithmetic arith

toMemSegment :: String -> MemSegment
toMemSegment seg = case seg of
                     "local" -> Local
                     "argument" -> Argument
                     "this" -> This
                     "that" -> That
                     "constant" -> Constant
                     "static" -> Static
                     "temp" -> Temp
                     "pointer" -> Pointer

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

lineP :: ReadP Line
lineP = do
  cmd <- popP <|> pushP <|> arithmeticP
  option Nothing $ fmap Just $ satisfy isSpace
  memSeg <- option Nothing $ fmap Just memSegmentP
  option Nothing $ fmap Just $ satisfy isSpace
  idx <- option Nothing $ fmap Just indexP
  return (Line cmd memSeg idx)
