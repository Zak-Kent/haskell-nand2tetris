module VMWriter where

import qualified Control.Monad.State as S
import Text.Printf
import Data.Maybe
import VMParser


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

translateLabel :: Command -> LabelCountState [String]
translateLabel (Label l) = return ["(" ++ l ++ ")"]

translateGoto :: Command -> LabelCountState [String]
translateGoto (Goto l) = return ["@" ++ l, "0;JMP"]

translateIfGoto :: Command -> LabelCountState [String]
translateIfGoto (IfGoto l) = return ["@SP", "AM=M-1", "D=M", "@" ++ l, "D;JGT"]

translateLine :: Line -> LabelCountState [String]
translateLine line@(Line {command = c}) = case c of
    Push -> return (translatePush line)
    Pop -> return (translatePop line)
    Arithmetic _ -> translateArithmetic c
    Label _ -> translateLabel c
    Goto _ -> translateGoto c
    IfGoto _ -> translateIfGoto c

translateFile :: [Line] -> String
translateFile ls = unlines $ (concat $ S.evalState (mapM translateLine ls) 0) ++ endProg
