{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module CodeGen where

import Data.Char (ord)
import qualified Control.Monad.State as S
import qualified Data.Map as M
import Text.Printf
import Data.Maybe

import AST
import SymbolTable

type SymTable = M.Map VarName SymbolInfo
type WhileLC = Int -- used to generate unique labels in flow control
type IfLC = Int
type ClassName = String
-- the two symbols tables needed at any given point in compilation
-- (<class symbol table>, <subroutine symbol table>)
type SymbolTableState = S.State (SymTable, SymTable, WhileLC, IfLC, ClassName)

genCmds :: [SymbolTableState String] -> SymbolTableState String
genCmds cmds = fmap concat $ sequence cmds

lookupSym :: VarName -> SymbolTableState (Maybe SymbolInfo)
lookupSym vn = do
  s <- S.get
  return $ checkVal vn s
  where checkVal v (clsSyms, localSyms, _, _, _) =
          case M.lookup v localSyms of
            (Just symI) -> (Just symI)
            Nothing -> case M.lookup v clsSyms of
              Nothing -> Nothing
              (Just symI) -> (Just symI)

genSymCmd :: SymbolInfo -> String
-- all class fields are mapped to 'this' with an offset
genSymCmd (SymbolInfo _ (Keyword "field") occ) = printf "this %d\n" occ
genSymCmd (SymbolInfo _ (Keyword k) occ) = printf "%s %d\n" k occ

class VMGen a where
  genVM :: a -> SymbolTableState String

instance VMGen Symbol where
  genVM (Symbol s) = return s

instance VMGen Keyword where
  genVM (Keyword k) = return k

instance VMGen Identifier where
  genVM i@(Identifier idf) = do
    symI <- lookupSym i
    return $ symCmd symI
      where symCmd si =
              case si of
                Nothing -> error
                  $ printf "error: '%s' not found in symbol tables" idf
                (Just s) -> genSymCmd s

instance VMGen SubCall where
  genVM (SubCallName (Identifier sn) exprs) = do
    (_, _, _, _, clsName) <- S.get
    genCmds [pure "push pointer 0\n",
             genVM exprs,
             (pure $ printf "call %s.%s %d\n" clsName sn ((+1) (length exprs)))]
  genVM (SubCallClassOrVar cvn (Identifier subN) exprs) = do
    -- if the first name is in the symbol table sub out for Class type
    -- ex. var Baz b1; do b1.foo(); -> call Baz.foo 1
    clsSym <- lookupSym cvn
    genCmds [genVM exprs, pure $ genSubCmds clsSym]
    where genSubCmds clsSym =
            case clsSym of
              Nothing -> genCall cvn (length exprs)
              symI@(Just (SymbolInfo
                          (TIdentifier tp) _ _)) ->
                "push " ++  genSymCmd (fromJust symI)
                ++ genCall tp ((+1) (length exprs))
              _ -> error "SymbolInfo with a TKeyword encountered"

          genCall (Identifier name) nExprs =
            printf "call %s.%s %d\n" name subN nExprs

instance VMGen Term where
  genVM (IntegerConstant i) = return $ printf "push constant %d\n" i
  genVM (ParenExpr expr) = genVM expr
  genVM (SubroutineCall sc) = genVM sc
  genVM (VarName vn) = genCmds [pure "push ", genVM vn]

  genVM (VarNameExpr vn expr) =
    -- This is for handling array access on the right side of an '='
    -- Ex. let foo = a[1];
    -- in this case the value at a[1] needs to be pushed on to the stack
    genCmds [
      genVM expr,
      pure "push ",
      genVM vn,
      pure "add\n",
      pure "pop pointer 1\n",
      pure "push that 0\n"
      ]

  genVM (StringConstant s) =
    genCmds $ [pure $ printf "push constant %d\n" (length s),
               pure "call String.new 1\n"]
               ++ (map genCharPush s)
    where
      genCharPush c = pure $
            printf "push constant %d\ncall String.appendChar 2\n" (ord c)

  genVM (KeywordConstant k) = return $ printf "push %s\n" (genKeyConsts k)
    where genKeyConsts kw = case M.lookup kw keywordConsts of
            Nothing -> kw
            (Just cmd) -> cmd
          keywordConsts = M.fromList [("null", "constant 0"),
                                      ("false", "constant 0"),
                                      ("true", "constant 0\nnot")]

  genVM (UnaryOp op t) = genCmds [genVM t, genUnaryOpSym op]
    where genUnaryOpSym o = case M.lookup o unaryOpSyms of
            Nothing -> genVM o
            (Just neg) -> return neg
          -- handle things like: -z or ~z in params. Ex. g(a, b, -z)
          unaryOpSyms = M.fromList [((Symbol "-"), "neg\n"),
                                    ((Symbol "~"), "not\n")]

  genVM (Op (Symbol s)) = case M.lookup s opSyms of
                            Nothing -> error
                              $ printf "%s operator not found in op lookup" s
                            (Just cmd) -> return cmd
    where opSyms = M.fromList [("+", "add\n"), ("-", "sub\n"),
                               ("*", "call Math.multiply 2\n"),
                               ("/", "call Math.divide 2\n"),
                               ("<", "lt\n"), (">", "gt\n"),
                               ("=", "eq\n"), ("&", "and\n")]

postOrderExpr :: Tree Term -> SymbolTableState String
postOrderExpr (Leaf t) = genVM t
postOrderExpr (Node lb op rb) =
  genCmds [postOrderExpr lb, postOrderExpr rb, genVM op]

instance VMGen Expr where
  genVM (Expr expr) = postOrderExpr expr

instance VMGen [Expr] where
  -- TODO: double check if you need to add \n between the lists here
  genVM exprs = genCmds $ map genVM exprs

instance VMGen [Statement] where
  genVM stmts = genCmds $ map genVM stmts

instance VMGen Else where
  genVM (Else stmts) = genVM stmts

incLabelCount :: String -> SymbolTableState Int
incLabelCount s = do
  (clsSyms, subSyms, wLC, iLC, cName) <- S.get
  case s of
    "if" -> S.put (clsSyms, subSyms, wLC, (iLC + 1), cName)
    "while" -> S.put (clsSyms, subSyms, (wLC + 1), iLC, cName)
    _ -> error "incLabelCount called with invalid input"
  return $ case s of
             "if" -> iLC
             "while" -> wLC

genReturn :: Maybe Expr -> SymbolTableState String
genReturn Nothing =
  genCmds [pure "push constant 0\n", pure "return\n"]
genReturn (Just (Expr (Leaf (KeywordConstant "this")))) =
  genCmds [pure "push pointer 0\n", pure "return\n"]
genReturn (Just cmd) = genCmds [genVM cmd, pure "return\n"]

genMaybeCmd :: (VMGen a) => Maybe a -> SymbolTableState String
genMaybeCmd Nothing = pure ""
genMaybeCmd (Just cmd) = genVM cmd

instance VMGen Statement where
  genVM (Let (LetVarName vn) expr) =
    genCmds [genVM expr, pure "pop ", genVM vn]
  genVM (Let (LetVarNameExpr lvn lExpr) expr) =
    genCmds [
      genVM lExpr,
      pure "push ",
      genVM lvn,
      pure "add\n",
      genVM expr,
      pure "pop temp 0\n",
      pure "pop pointer 1\n",
      pure "push temp 0\n",
      pure "pop that 0\n"
      ]

  -- this assumes that all do calls don't care about return value
  genVM (Do subCall) = genCmds [genVM subCall, pure "pop temp 0\n"]

  genVM (Return maybeExpr) = genReturn maybeExpr

  genVM (If expr stmts maybeStmts) = do
    lc <- incLabelCount "if"
    genCmds [
      genVM expr,
      pure $ printf "if-goto IF_TRUE%d\n" lc,
      pure $ printf "goto IF_FALSE%d\n" lc,
      pure $ printf "label IF_TRUE%d\n" lc,
      genVM stmts,
      pure $ genIfEnds maybeStmts (printf "goto IF_END%d\n" lc),
      pure $ printf "label IF_FALSE%d\n" lc,
      genMaybeCmd maybeStmts,
      pure $ genIfEnds maybeStmts (printf "label IF_END%d\n" lc)
      ]
      where genIfEnds mStmts cmd = case mStmts of
              Nothing -> ""
              (Just _) -> cmd

  genVM (While expr stmts) = do
    lc <- incLabelCount "while"
    genCmds [
      pure $ printf "label WHILE_EXP%d\n" lc,
      genVM expr,
      pure "not\n",
      pure $ printf "if-goto WHILE_END%d\n" lc,
      genVM stmts,
      pure $ printf "goto WHILE_EXP%d\n" lc,
      pure $ printf "label WHILE_END%d\n" lc
      ]

instance VMGen Type where
  genVM (TKeyword kw) = genVM kw
  genVM (TIdentifier i) = genVM i

mergeSyms :: [(VarName, SymbolInfo)] -> SymTable -> SymTable
mergeSyms syms symTb =
  foldr insert symTb syms
  where insert (v, si) m = M.insert v si m

occCount :: Keyword -> SymTable -> Int
occCount kw symT = length $ M.filter only symT
  where only SymbolInfo{kind=k} = kw == k

instance VMGen VarDec where
  {-
   VarDec blocks are only encountered in subroutines after any parameters for
   the subroutine have already been added to the subroutine SymbolTable. Any
   symbols encountered at this level are of kind 'local'. This function calcs
   the occurence based on any existing 'local' symbols created by other VarDecs
   and creates an entry for the symbols. No VM code is produced for VarDecs, only
   the subroutine SymbolTable state is updated. Clearing out the subroutine
   SymbolTable is handled a couple levels up in SubroutineDec.
  -}
  genVM (VarDec tp vn vns) = do
    (clsSyms, subSyms, wLC, iLC, cName) <- S.get
    let newSubSyms = mergeSyms (newSyms subSyms) subSyms
    S.put (clsSyms, newSubSyms, wLC, iLC, cName)
    return ""
    where newSyms subS = map symInfo
            $ zip (vn:vns) [(occCount (Keyword "local") subS)..]
          symInfo (v, oc) = (v,
                             SymbolInfo {typ = tp,
                                         kind = Keyword "local",
                                         occurrence = oc})

instance VMGen [VarDec] where
  genVM varDecs = genCmds $ map genVM varDecs

instance VMGen SubroutineBody where
  genVM (SubroutineBody varDecs stmts) = genCmds [genVM varDecs, genVM stmts]

instance VMGen ParameterList where
  genVM (ParameterList params) = do
    {-
     Every item in a parameter list is of kind 'argument'. It's possible
     in the case of a Jack method that an entry for 'this' may already be
     in the subroutine symbol table, this is accounted for by counting the
     keys below. Clearing of the symbol table is handled one level above in
     SubroutineDec.
    -}
    (clsSyms, subSyms, wLC, iLC, cName) <- S.get
    let newSubSyms = mergeSyms (newSyms subSyms) subSyms
    S.put (clsSyms, newSubSyms, wLC, iLC, cName)
    return ""
    where newSyms subS = map symInfo $ zip params [(length subS)..]
          symInfo ((tp, vn), oc) = (vn,
                                    SymbolInfo {typ = tp,
                                                kind = Keyword "argument",
                                                occurrence = oc})

initSubSymTbl :: String -> SymbolTableState String
initSubSymTbl methTyp = do
  (clsSyms, _, _, _, cName) <- S.get
  S.put (clsSyms, (subSyms methTyp cName), 0, 0, cName)
  return ""
  where subSyms mt cn = case mt of
          "method" ->
            M.fromList [((Identifier "this"),
                          SymbolInfo {typ = (TIdentifier
                                             (Identifier cn)) ,
                                      kind = Keyword "argument",
                                      occurrence = 0})]
          _ -> M.empty

genVMFuncDec :: Identifier -> SymbolTableState String
genVMFuncDec (Identifier subName) = do
  (_, subSyms, _, _, cName) <- S.get
  -- ex. 'function <className>.<subName> <num local vars>'
  return $ printf "function %s.%s %d\n" cName subName
         $ occCount (Keyword "local") subSyms

classFieldCount :: SymbolTableState String
classFieldCount = do
  (clsSyms, _, _, _, _) <- S.get
  return $ show $ occCount (Keyword "field") clsSyms

instance VMGen SubroutineDec where
  genVM (SubroutineDec (Keyword "method") _ sn params sb) = do
    _ <- initSubSymTbl "method"
    body <- genCmds [genVM params, genVM sb]
    funcDec <- genVMFuncDec sn
    return $ concat [
      funcDec,
      -- set 'this' which is passed as arg 0 to every method
     "push argument 0\npop pointer 0\n",
      body
      ]

  genVM (SubroutineDec (Keyword "constructor") _ sn params sb) = do
    _ <- initSubSymTbl "constructor"
    body <- genCmds [genVM params, genVM sb]
    fieldCount <- classFieldCount
    funcDec <- genVMFuncDec sn
    return $ concat [
      funcDec,
      printf "push constant %s\n" fieldCount :: String,
      "call Memory.alloc 1\n",
      "pop pointer 0\n",
      body
      ]

  genVM (SubroutineDec (Keyword "function") _ sn params sb) = do
    _ <- initSubSymTbl "function"
    -- order matters here, body needs to be calculated first because
    -- the genVM calls below this level populate the symbol table which
    -- is needed to get the number of fields for the VM function declaration
    body <- genCmds [genVM params, genVM sb]
    funcDec <- genVMFuncDec sn
    return $ concat [funcDec, body]

instance VMGen [SubroutineDec] where
  genVM subVarDecs = genCmds $ map genVM subVarDecs

instance VMGen ClassVarDec where
  genVM (ClassVarDec kw tp vn vns) = do
    -- clearing out class symbol table happens one level up in Class
    (clsSyms, subSyms, wLC, iLC, cName) <- S.get
    let newClsSyms = mergeSyms (newSyms clsSyms) clsSyms
    S.put (newClsSyms, subSyms, wLC, iLC, cName)
    return ""
    where newSyms cSyms = map symInfo $ zip (vn:vns) [(length cSyms)..]
          symInfo (v, oc) = (v,
                             SymbolInfo {typ = tp,
                                         kind = kw,
                                         occurrence = oc})

instance VMGen [ClassVarDec] where
  genVM clsVarDecs = genCmds $ map genVM clsVarDecs

instance VMGen Class where
  genVM (Class _ (Identifier clsName) clsVarDecs subDecs) = do
    S.put (M.empty, M.empty, 0, 0, clsName) -- init empty state
    genCmds [genVM clsVarDecs, genVM subDecs]
