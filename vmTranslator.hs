import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)
import Data.Char (isSpace)

data Command = Arithmetic String | Push | Pop
             deriving (Show)
data MemSegment = Local Integer | Argument Integer | This Integer
                | That Integer | Constant | Static Integer
                | Temp Integer | Pointer Integer
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

arithmeticP :: ReadP Line
arithmeticP = do
  arith <- string "add" <|> string "sub" <|> string "neg" <|> string "eq" <|>
           string "gt" <|> string "lt" <|> string "and" <|> string "or" <|>
           string "not"
  return (Line (Arithmetic arith) Nothing Nothing)

toMemSegment :: String -> MemSegment
toMemSegment seg = case seg of
                     "local" -> Local 1
                     "argument" -> Argument 2
                     "this" -> This 3
                     "that" -> That 4
                     "constant" -> Constant
                     "static" -> Static 16
                     "temp" -> Temp 5
                     "pointer" -> Pointer 3

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
  satisfy isSpace
  memSeg <- fmap Just memSegmentP
  satisfy isSpace
  idx <- fmap Just indexP
  return (Line cmd memSeg idx)

lineP :: ReadP Line
lineP = do
  line <- arithmeticP <|> pushPopP
  return line

-- VM -> hack assembly

incSP :: [String]
incSP = ["@SP // SP++", "M=M+1"]

decSP :: [String]
decSP = ["@SP // SP--", "M=M-1"]

addressMemSeg :: Line -> [String]
addressMemSeg (Line {memSegment = Just m, index = Just i}) =
  case m of
    Local b -> addr b
    Argument b -> addr b
    This b -> addr b
    That b -> addr b
    Constant -> ["@" ++ show i]
    Static b -> addr b
    Temp b -> addr b
    Pointer b -> addr b
  where addr = (\b -> ["@" ++ show (b + i)])

setDRegWA :: [String]
setDRegWA = ["D=A"]

setMRegWD :: [String]
setMRegWD = ["M=D"]

setDRegWM :: [String]
setDRegWM = ["D=M"]

addressMReg :: [String]
addressMReg = ["A=M"]

pushVal :: [String]
-- assumes value to push on stack is in reg D
pushVal = ["@SP // *SP=D", "A=M", "M=D"]

translateLine :: Line -> [String]
translateLine line@(Line {command = c}) =
  case c of
    Push -> ["// push val"] ++ addressMemSeg line ++ setDRegWA ++ pushVal ++ incSP
    Pop -> ["// pop val"] ++ decSP ++ addressMReg ++ setDRegWM ++ addressMemSeg line
           ++ addressMReg ++ setMRegWD
    Arithmetic c -> ["arith"]

