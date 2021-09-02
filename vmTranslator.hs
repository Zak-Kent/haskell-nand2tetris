import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)
import Data.Char (isSpace)

-- TODO: will need to break this out into seperate files later on


-- VM language parser

data Command = Arithmetic String | Push | Pop
             deriving (Show)
data MemSegment = Local | Argument | This | That | Constant | Static | Temp | Pointer
                deriving (Show)
type Index = Integer

data Line = Line { command :: Command,
                   memSegment :: MemSegment,
                   index :: Index
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
  art <- string "add" <|> string "sub" <|> string "neg" <|> string "eq" <|>
         string "gt" <|> string "lt" <|> string "and" <|> string "or" <|>
         string "not"
  return $ Arithmetic art

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
  satisfy isSpace
  memSeg <- memSegmentP
  satisfy isSpace
  idx <- indexP
  return (Line cmd memSeg idx)
