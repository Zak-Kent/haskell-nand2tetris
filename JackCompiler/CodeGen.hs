{-# LANGUAGE FlexibleInstances #-}
module CodeGen where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Text.Printf

import AST
import SymbolTable

type SymTable = M.Map VarName SymbolInfo
-- the two symbols tables needed at any given point in compilation
-- (<class symbol table>, <subroutine symbol table>)
type SymbolTableState = S.State (SymTable, SymTable)

class VMGen a where
  genVM :: a -> SymbolTableState String

instance VMGen Symbol where
  genVM (Symbol s) = return s

instance VMGen Keyword where
  genVM (Keyword k) = return k

instance VMGen Identifier where
  genVM (Identifier i) = return i

instance VMGen Term where
  genVM (IntegerConstant i) = return $ printf "push %d\n" i
  genVM (StringConstant s) = return $ printf "push %s\n" s
  genVM (KeywordConstant k) = return $ printf "push %s\n" k
  genVM (VarName vn) = do
    s <- S.get
    return $ printf "push %s\n" (S.evalState (genVM vn) s)
  genVM (UnaryOp op t) = do
    s <- S.get
    return $ printf "%s\n %s\n" (S.evalState (genVM t) s) (S.evalState (genVM op) s)
  genVM (VarNameExpr vn (Expr expr)) = do
    s <- S.get
    return $ S.evalState (postOrderExpr expr) s
      ++ printf "call %s\n" (S.evalState (genVM vn) s)
  genVM (ParenExpr (Expr expr)) = do
    s <- S.get
    return $ S.evalState (postOrderExpr expr) s
  genVM (SubroutineCall (SubCallName sn exprs)) = do
    s <- S.get
    return $ (concat $ S.evalState (mapM genVM exprs) s)
      ++ (printf "call %s\n" (S.evalState (genVM sn) s)) -- (genVM sn)
  genVM (SubroutineCall (SubCallClassOrVar cvn sn exprs)) = do
    s <- S.get
    return $ (concat $ S.evalState (mapM genVM exprs) s)
      ++ printf "call %s.%s" (S.evalState (genVM cvn) s) (S.evalState (genVM sn) s)
  genVM (Op s) = do
    st <- S.get
    return $ printf "%s\n" (S.evalState (genVM s) st)

postOrderExpr :: Tree Term -> SymbolTableState String
postOrderExpr (Leaf t) = genVM t
postOrderExpr (Node lb op rb) = do
  s <- S.get
  return $ (S.evalState (postOrderExpr lb) s)
    <> (S.evalState (postOrderExpr rb) s)
    <> (S.evalState (genVM op) s)

instance VMGen Expr where
  genVM (Expr expr) = do
    s <- S.get
    return $ S.evalState (postOrderExpr expr) s

instance VMGen [Expr] where
  -- TODO: double check if you need to add \n between the lists here
  genVM exprs = do
    s <- S.get
    return $ concat $ S.evalState (mapM genVM exprs) s
