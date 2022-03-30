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

translatePop :: String -> Line ->  [String]
translatePop fName (Line {memSegment = ms, index = Just i}) =
  case (segName memSeg, segType memSeg) of
    (Constant, _) -> ["There should never be a pop constant command"]
    (_, Point) -> transPointer ba
    (Static, Fixed) -> transStatic
    (_, Fixed) -> transFixed ba
  where memSeg = fromJust ms
        ba = fromJust $ baseAddr memSeg
        transFixed = (\b -> popValOffStackToDReg ++ ["@" ++ show (b + i)] ++ setMRegWD)
        transStatic = popValOffStackToDReg ++ ["@" ++ fName ++ "$" ++ show i] ++ setMRegWD
        transPointer = (\b ->
                        -- put address reg in R13
                        ["@" ++ show b, "D=M", "@" ++ show i, "D=D+A", "@R13", "M=D"]
                        ++ popValOffStackToDReg ++ ["@R13", "A=M", "M=D"])

translatePush :: String -> Line ->  [String]
translatePush fName (Line {memSegment = ms, index = Just i}) =
  case (segName memSeg, segType memSeg) of
    (Constant, _) -> ["@" ++ show i] ++ setDRegWA ++ pushVal
    (_, Point) -> transPointer ba
    (Static, Fixed) -> transStatic
    (_, Fixed) -> transFixed ba
  where memSeg = fromJust ms
        ba = fromJust $ baseAddr memSeg
        transFixed = (\b -> ["@" ++ show (b + i)] ++ setDRegWM ++ pushVal)
        transStatic = ["@" ++ fName ++ "$" ++ show i] ++ setDRegWM ++ pushVal
        transPointer = (\b -> ["@" ++ show b, "D=M", "@" ++ show i, "A=D+A"]
                       ++ setDRegWM ++ pushVal)

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
pushVal = ["@SP // *SP=D", "A=M", "M=D"] ++ incSP

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

translateCall :: Command -> LabelCountState [String]
translateCall (Call funcName nArgs) = do
  -- need to generate a unique return address label for each func call
  callCount <- S.get
  S.put (callCount + 1)
  let retAddr = printf (funcName ++ ".call" ++ "$" ++ "%d") callCount
  return (
    -- push retAddr onto stack
    ["@" ++ retAddr] ++ setDRegWA ++ pushVal
    -- push caller's *LCL onto stack
    ++ ["@LCL"] ++ setDRegWM ++ pushVal
    -- push caller's *ARG onto stack
    ++ ["@ARG"] ++ setDRegWM ++ pushVal
    -- push caller's *THIS onto stack
    ++ ["@THIS"] ++ setDRegWM ++ pushVal
    -- push caller's *THAT onto stack
    ++ ["@THAT"] ++ setDRegWM ++ pushVal
    -- set push *SP and 5 to stack and sub
    ++ ["@SP"] ++ setDRegWM ++ pushVal ++ ["@5"] ++ setDRegWA ++ pushVal ++ sub
    -- push nArgs to stack and sub
    ++ ["@" ++ show nArgs] ++ setDRegWA ++ pushVal ++ sub
    -- pop val to D reg and set *ARG to val
    ++ popValOffStackToDReg ++ ["@ARG"] ++ setMRegWD
    -- set LCL = *SP
    ++ ["@SP"] ++ setDRegWM ++ ["@LCL"] ++ setMRegWD
    -- goto function
    ++ ["@" ++ funcName, "0;JMP"]
    -- declare retAddr label
    ++ ["(" ++ retAddr ++ ")"]
    )
  where sub = twoArgBase ++ ["M=M-D"]

translateFunction :: Command -> LabelCountState [String]
translateFunction (Function funcName nLocals) =
  return (
    ["(" ++ funcName ++ ")"] -- declare label for func
    ++ pushLocals (fromInteger nLocals) -- push 0 to stack nLocals times
  )
  where pushLocals = (\n -> concat
                       (replicate n (["@0"] ++ setDRegWA ++ pushVal)))

translateReturn :: Command -> LabelCountState [String]
translateReturn Return =
  return (
    -- save LCL into temp 'endFrame' var using R14
    ["@LCL"] ++ setDRegWM ++ ["@R14"] ++ setMRegWD
    -- save 'retAddr' in var using R15
    ++ (calcEndFrameOffset "5") ++ ["@R15"] ++ setMRegWD
    -- set *ARG = pop (), this is the top of the stack of the caller
    ++ popValOffStackToDReg ++ ["@ARG"] ++ addressMReg ++ setMRegWD
    -- set SP = ARG + 1
    ++ ["@ARG"] ++ setDRegWM ++ ["@1"] ++ ["D=D+A"] ++ ["@SP"] ++ setMRegWD
    -- THAT = *(endFrame - 1)
    ++ (calcCallerAddr "THAT" "1")
    -- THIS = *(endFrame - 2)
    ++ (calcCallerAddr "THIS" "2")
    -- ARG = *(endFrame - 3)
    ++ (calcCallerAddr "ARG" "3")
    -- LCL = *(endFrame - 4)
    ++ (calcCallerAddr "LCL" "4")
    -- get retAddr in R15 and goto
    ++ ["@R15"] ++ addressMReg ++ ["0;JMP"]
  )
  where calcEndFrameOffset offset =
          ["@R14"] ++ setDRegWM ++ [(printf "@%s" offset)] ++ ["D=D-A"]
          ++ ["A=D"] ++ ["D=M"]
        calcCallerAddr memSeg offset =
          (calcEndFrameOffset offset) ++ ["@" ++ memSeg] ++ setMRegWD

translateLabel :: Command -> LabelCountState [String]
translateLabel (Label l) = return ["(" ++ l ++ ")"]

translateGoto :: Command -> LabelCountState [String]
translateGoto (Goto l) = return ["@" ++ l, "0;JMP"]

translateIfGoto :: Command -> LabelCountState [String]
translateIfGoto (IfGoto l) = return ["@SP", "AM=M-1", "D=M", "@" ++ l, "D;JNE"]

translateLine :: String -> Line -> LabelCountState [String]
translateLine fName line@(Line {command = c}) = case c of
    Push -> return (translatePush fName line)
    Pop -> return (translatePop fName line)
    Arithmetic _ -> translateArithmetic c
    Label _ -> translateLabel c
    Goto _ -> translateGoto c
    IfGoto _ -> translateIfGoto c
    Call _ _ -> translateCall c
    Function _ _ -> translateFunction c
    Return -> translateReturn c

translateFile :: String -> [Line] -> String
translateFile fName ls =
  unlines $
  (concat $ S.evalState (mapM (translateLine fName) ls) 0)
