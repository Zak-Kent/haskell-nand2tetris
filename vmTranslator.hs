{-# LANGUAGE DeriveDataTypeable #-}

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)
import qualified Control.Monad.State as S
import Data.Char (isSpace, isAlpha)
-- import System.Console.CmdArgs
import Data.Maybe
import System.IO
import System.Environment
import Text.Printf
import System.FilePath.Posix(replaceExtensions)

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

checkParse :: [(a, b)] -> Maybe a
checkParse [] = Nothing
checkParse (x:_) = Just (fst x)

parseLines :: [String] -> [Line]
-- type of 'map readP_toS lines' :: [[(Line, String)]]
parseLines l = catMaybes $ map checkParse $ map (readP_to_S lineP) l

-- VM -> hack assembly

incSP :: [String]
incSP = ["@SP // SP++", "M=M+1"]

decSP :: [String]
decSP = ["@SP // SP--", "M=M-1"]

addressTopStack :: [String]
addressTopStack = ["@SP", "A=M-1"]

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

endProg :: [String]
endProg = ["(end)", "@end", "0;JMP"]

translateLine :: Line -> LabelCountState [String]
translateLine line@(Line {command = c}) = case c of
    Push -> return (["// push val"] ++ addressMemSeg line
                   -- TODO need a branch here maybe. If pushing a constant you want the value from A
                   -- if pushing from a mem seg you want the value at the mem location
                    ++ setDRegWA ++ pushVal ++ incSP)
    -- need to account for the fact that static, temp, pointer can be imlemented with a static base address
    -- but the other registers need to look up the address in the memory slot and then set that which means
    -- you'll need to have a condional here that knows how to deal with the diff memsegments. The impl
    -- below assumes you can address all the values with a static address
    Pop -> return (["// pop val"] ++ decSP ++ addressMReg
                   ++ setDRegWM ++ addressMemSeg line ++ setMRegWD)
    Arithmetic _ -> translateArithmetic c

  -- where outy = S.evalState (mapM funky l) 0
translateFile :: [Line] -> String
translateFile ls = unlines $ (concat $ S.evalState (mapM translateLine ls) 0) ++ endProg

-- data VMTranslatorArgs = VMTranslatorArgs {
--   src :: FilePath
--   ,dst :: FilePath
--   } deriving (Data,Typeable,Show)

-- options :: VMTranslatorArgs
-- options = VMTranslatorArgs {
--   src = "abc.txt" &= help "path to input Hack VM file"
--   ,dst = "dst.asm" &= help "path where translated assembly will be saved"
--   } &= program "VM -> assembly translator"

parseFileName :: ReadP String
parseFileName = do
  fileName <- many1 (satisfy (\ch -> isAlpha ch && ch /= '.'))
  return fileName

main :: IO ()
main = do
  -- args <- cmdArgs options
  -- let srcFile = src args
  --     dstFile = dst args

  args <- getArgs
  let srcFile = head args
      dstFile = (fst $ last $ readP_to_S parseFileName $ srcFile) ++ ".asm"

  contents <- fmap lines $ readFile srcFile
  let output = translateFile $ parseLines contents

  outFile <- openFile dstFile WriteMode
  hPutStrLn outFile output
  hClose outFile
