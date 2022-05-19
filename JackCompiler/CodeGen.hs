{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module CodeGen where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Text.Printf

import AST
import SymbolTable

type SymTable = M.Map VarName SymbolInfo
type LabelCount = Int -- used to generate unique labels in flow control
type ClassName = String
-- the two symbols tables needed at any given point in compilation
-- (<class symbol table>, <subroutine symbol table>)
type SymbolTableState = S.State (SymTable, SymTable, LabelCount, ClassName)

genMaybeCmd :: (VMGen a) => Maybe a -> SymbolTableState String
genMaybeCmd Nothing = pure ""
genMaybeCmd (Just cmd) = genVM cmd

genCmds :: [SymbolTableState String] -> SymbolTableState String
genCmds cmds = fmap concat $ sequence cmds

lookupSym :: VarName -> SymbolTableState (Maybe SymbolInfo)
lookupSym vn = do
  s <- S.get
  return $ checkVal vn s
  where checkVal v (clsSyms, localSyms, _, _) =
          case M.lookup v localSyms of
            (Just symI) -> (Just symI)
            Nothing -> case M.lookup v clsSyms of
              Nothing -> Nothing
              (Just symI) -> (Just symI)

genSymCmd :: SymbolInfo -> String
-- TODO: figure out where push vs. pop happens depending on context
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
  -- TODO: you many need to add VM cmds here to pop off the dummy
  -- return value if any void methods are called
  genVM (SubCallName (Identifier sn) exprs) =
    (++) <$> genVM exprs <*> (pure $ printf "call %s\n" sn)
  genVM (SubCallClassOrVar (Identifier cvn) (Identifier sn) exprs) =
    (++) <$> genVM exprs <*> (pure $ printf "call %s.%s\n" cvn sn)

instance VMGen Term where
  genVM (IntegerConstant i) = return $ printf "push constant %d\n" i
  genVM (StringConstant s) = return $ printf "push %s\n" s
  genVM (KeywordConstant k) = return $ printf "push %s\n" k
  genVM (VarName vn) = genCmds [pure "push ", genVM vn]
  genVM (UnaryOp op t) = genCmds [genVM t, genVM op]
  genVM (VarNameExpr vn expr) =
    (++) <$> genVM expr <*> genCmds [pure "call ", genVM vn]
  genVM (ParenExpr expr) = genVM expr
  genVM (SubroutineCall sc) = genVM sc
  genVM (Op s) = genVM s

postOrderExpr :: Tree Term -> SymbolTableState String
postOrderExpr (Leaf t) = genVM t
postOrderExpr (Node lb op rb) =
  genCmds [postOrderExpr lb, postOrderExpr rb, genVM op]

instance VMGen Expr where
  genVM (Expr expr) = postOrderExpr expr

instance VMGen [Expr] where
  -- TODO: double check if you need to add \n between the lists here
  genVM exprs = genCmds $ map genVM exprs

instance VMGen LetVarName where
  genVM (LetVarName vn) = genVM vn
  genVM (LetVarNameExpr vn expr) =
    -- TODO: this is behavior is wrong, fix when you learn how
    -- to handle array access
    genCmds [genVM expr, pure "pop ", genVM vn]

instance VMGen [Statement] where
  genVM stmts = genCmds $ map genVM stmts

instance VMGen Else where
  genVM (Else stmts) = genVM stmts

incLabelCount :: SymbolTableState (Int, Int)
incLabelCount = do
  (clsSyms, subSyms, labelCount, cName) <- S.get
  S.put (clsSyms, subSyms, (labelCount + 2), cName)
  return (labelCount + 1, labelCount + 2)

instance VMGen Statement where
  genVM (Let vn expr) =
    genCmds [genVM expr, pure "pop ", genVM vn]

  genVM (Do subCall) = genVM subCall

  genVM (Return maybeExpr) =
    genCmds [genMaybeCmd maybeExpr, pure "return\n"]

  genVM (If expr stmts maybeStmts) = do
      (l1, l2) <- incLabelCount
      genCmds [
        genVM expr,
        pure "push not\n",
        pure $ printf "if-goto L%d\n" l1,
        genVM stmts,
        pure $ printf "goto L%d\n" l2,
        pure $ printf "label L%d\n" l1,
        genMaybeCmd maybeStmts,
        pure $ printf "label L%d\n" l2
        ]

  genVM (While expr stmts) = do
    (l1, l2) <- incLabelCount
    genCmds [
      pure $ printf "label L%d\n" l1,
      genVM expr,
      pure "push not\n",
      pure $ printf "if-goto L%d\n" l2,
      genVM stmts,
      pure $ printf "goto L%d\n" l1,
      pure $ printf "label L%d\n" l2
      ]

instance VMGen Type where
  genVM (TKeyword kw) = genVM kw
  genVM (TIdentifier i) = genVM i

addSyms :: [(VarName, SymbolInfo)] -> SymTable -> SymTable
addSyms syms symTb =
  foldr insert symTb syms
  where insert (v, si) m = M.insert v si m

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
    (clsSyms, subSyms, labelC, cName) <- S.get
    let newSubSyms = addSyms (newSyms subSyms) subSyms
    S.put (clsSyms, newSubSyms, labelC, cName)
    return ""
    where newSyms subS = map symInfo $ zip (vn:vns) [(occCount subS)..]
          symInfo (v, oc) = (v,
                             SymbolInfo {typ = tp,
                                         kind = Keyword "local",
                                         occurrence = oc})
          occCount subS = length $ M.keys $ M.filter localOnly subS
          localOnly SymbolInfo{kind=k} = Keyword "local" == k

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
    (clsSyms, subSyms, labelC, cName) <- S.get
    let newSubSyms = addSyms (newSyms subSyms) subSyms
    S.put (clsSyms, newSubSyms, labelC, cName)
    return ""
    where newSyms subS = map symInfo $ zip params [(occCount subS)..]
          symInfo ((tp, vn), oc) = (vn,
                                    SymbolInfo {typ = tp,
                                                kind = Keyword "argument",
                                                occurrence = oc})
          occCount subS = length $ M.keys subS
