import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)
import qualified Control.Monad.State as S
import Data.Char (isSpace)
import Data.Maybe
import System.IO
import System.Environment
import Text.Printf
import System.FilePath.Posix(replaceExtensions)

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
-}
data Command = Arithmetic String | Push | Pop
             deriving (Show)
type Index = Integer
data Line = Line { command :: Command,
                   memSegment :: Maybe MemSegment,
                   index :: Maybe Index
                 } deriving (Show)

-- VM command parsing --

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
  satisfy isSpace
  memSeg <- fmap Just memSegmentP
  satisfy isSpace
  idx <- fmap Just indexP
  return (Line cmd memSeg idx)

lineP :: ReadP Line
lineP = do
  line <- arithmeticP <|> pushPopP
  return line

checkParse :: [(a, b)] -> Maybe a
checkParse [] = Nothing
checkParse (x:_) = Just (fst x)

parseLines :: [String] -> [Line]
-- type of 'map readP_toS lines' :: [[(Line, String)]]
parseLines l = catMaybes $ map checkParse $ map (readP_to_S lineP) l

-- VM -> hack assembly --

incSP :: [String]
incSP = ["@SP // SP++", "M=M+1"]

decSP :: [String]
decSP = ["@SP // SP--", "M=M-1"]

addressTopStack :: [String]
addressTopStack = ["@SP", "A=M-1"]

popValOffStackToDReg :: [String]
popValOffStackToDReg = ["// pop val"] ++ decSP ++ addressMReg ++ setDRegWM

translatePop :: Line ->  [String]
translatePop (Line {memSegment = ms, index = Just i}) =
  case (segName memSeg, segType memSeg) of
    (Constant, _) -> ["There should never be a pop constant command"]
    (_, Point) -> transPointer ba
    (_, Fixed) -> transFixed ba
  where memSeg = fromJust ms
        ba = fromJust $ baseAddr memSeg
        transFixed = (\b -> popValOffStackToDReg ++ ["@" ++ show (b + i)] ++ setMRegWD)
        transPointer = (\b ->
                        -- put address reg in R13
                        ["@" ++ show b, "D=M", "@" ++ show i, "D=D+A", "@R13", "M=D"]
                        ++ popValOffStackToDReg ++ ["@R13", "A=M", "M=D"])

translatePush :: Line ->  [String]
translatePush (Line {memSegment = ms, index = Just i}) =
  case (segName memSeg, segType memSeg) of
    (Constant, _) -> ["@" ++ show i] ++ setDRegWA ++ pushVal ++ incSP
    (_, Point) -> transPointer ba
    (_, Fixed) -> transFixed ba
  where memSeg = fromJust ms
        ba = fromJust $ baseAddr memSeg
        transFixed = (\b -> ["@" ++ show (b + i)] ++ setDRegWM ++ pushVal ++ incSP)
        transPointer = (\b -> ["@" ++ show b, "D=M", "@" ++ show i, "A=D+A"]
                       ++ setDRegWM ++ pushVal ++ incSP)

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

endProg :: [String]
endProg = ["(end)", "@end", "0;JMP"]

-- Arithmetic commands
twoArgBase :: [String]
{- address top val & dec SP, set D to top val, address 2nd val.
   Run any following commands using D and value at M: ex. M=M+D
   storing the result in M which is the slot at the top of stack -}
twoArgBase = ["@SP", "AM=M-1", "D=M", "A=A-1"]

type LabelCount = Int
type LabelCountState = S.State LabelCount

compBase :: String -> LabelCountState [String]
{- compare top two values on stack depending on result
   jump to appropriate label and store Bool result back
   on stack
-}
compBase c = do
  callCount <- S.get
  S.put (callCount + 1)
  return (twoArgBase ++
             ["D=M-D",
              printf "@FALSE%d" callCount,
              ("D;" ++ c),
              "@SP",
              "A=M-1",
              "M=-1 // set top val to true, all 1s",
              printf "@CONT%d" callCount,
              "0;JMP",
              printf "(FALSE%d)" callCount,
              "@SP",
              "A=M-1",
              "M=0 // set top val to false, all 0s",
              printf "(CONT%d)" callCount])

translateArithmetic :: Command -> LabelCountState [String]
translateArithmetic (Arithmetic c) = case c of
  "add" -> return (twoArgBase ++ ["M=M+D"])
  "sub" -> return (twoArgBase ++ ["M=M-D"])
  "and" -> return (twoArgBase ++ ["M=M&D"])
  "or"  -> return (twoArgBase ++ ["M=M|D"])
  "eq"  -> compBase "JNE"
  "gt"  -> compBase "JLE"
  "lt"  -> compBase "JGE"
  "not" -> return (addressTopStack ++ ["M=!M"])
  "neg" -> return (["D=0"] ++ addressTopStack ++ ["M=D-M"])
translateArithmetic _ = return ["should never happen"]

translateLine :: Line -> LabelCountState [String]
translateLine line@(Line {command = c}) = case c of
    Push -> return (translatePush line)
    Pop -> return (translatePop line)
    Arithmetic _ -> translateArithmetic c

translateFile :: [Line] -> String
translateFile ls = unlines $ (concat $ S.evalState (mapM translateLine ls) 0) ++ endProg

getDaArgs :: [String] -> String
getDaArgs [] = "da fuq"
getDaArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  let srcFile = getDaArgs args

  if srcFile == "da fuq" then do
    putStrLn "no src file arg"
  else do
    let dstFile = replaceExtensions srcFile "asm"

    contents <- fmap lines $ readFile srcFile
    let output = translateFile $ parseLines contents

    outFile <- openFile dstFile WriteMode
    hPutStr outFile output
    hClose outFile
