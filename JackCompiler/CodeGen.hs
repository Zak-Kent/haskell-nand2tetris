{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module CodeGen where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Text.Printf

import AST
import SymbolTable

type SymTable = M.Map VarName SymbolInfo
type LabelCount = Int -- used to generate unique labels in flow control
-- the two symbols tables needed at any given point in compilation
-- (<class symbol table>, <subroutine symbol table>)
type SymbolTableState = S.State (SymTable, SymTable, LabelCount)

genMaybeCmd :: (VMGen a) => Maybe a -> SymbolTableState String
genMaybeCmd Nothing = pure ""
genMaybeCmd (Just cmd) = genVM cmd

genCmds :: [SymbolTableState String] -> SymbolTableState String
genCmds cmds = fmap concat $ sequence cmds

lookupSym :: VarName -> SymbolTableState (Maybe SymbolInfo)
lookupSym vn = do
  s <- S.get
  return $ checkVal vn s
  where checkVal v (clsSyms, localSyms, _) =
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
  genVM i = do
    symI <- lookupSym i
    return $ symCmd symI
      where symCmd si =
              case si of
                -- TODO: add better error reporting
                -- This will cause the VM commands to pop but,
                -- you could pop out of code gen when this happens
                Nothing -> "error: symbol not found"
                (Just s) -> genSymCmd s

instance VMGen SubCall where
  -- TODO: you many need to add VM cmds here to pop off the dummy
  -- return value if any void methods are called
  genVM (SubCallName (Identifier sn) exprs) =
    (++) <$> genVM exprs <*> (pure $ printf "call %s\n" sn)
  genVM (SubCallClassOrVar (Identifier cvn) (Identifier sn) exprs) =
    (++) <$> genVM exprs <*> (pure $ printf "call %s.%s\n" cvn sn)

instance VMGen Term where
  genVM (IntegerConstant i) = return $ printf "push %d\n" i
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

instance VMGen Statement where
  genVM (Let vn expr) =
    genCmds [genVM expr, pure "pop ", genVM vn]

  genVM (Do subCall) = genVM subCall

  genVM (Return maybeExpr) =
    genCmds [genMaybeCmd maybeExpr, pure "return\n"]

  genVM (If expr stmts maybeStmts) = do
      (clsSyms, subSyms, labelC) <- S.get
      S.put (clsSyms, subSyms, (labelC + 2))
      let l1 = labelC + 1; l2 = labelC + 2
      genCmds [
        genVM expr,
        pure "not\n",
        pure $ printf "if-goto L%d\n" l1,
        genVM stmts,
        pure $ printf "goto L%d\n" l2,
        pure $ printf "label L%d\n" l1,
        genMaybeCmd maybeStmts,
        pure $ printf "label L%d\n" l2
        ]

  genVM _ = undefined
