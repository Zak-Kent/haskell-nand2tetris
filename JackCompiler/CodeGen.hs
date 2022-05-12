{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
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

-- wrapper trick to get a list of heterogeneous items as args
data VM = forall a . (VMGen a) => VM a
data P = forall a. PrintfArg a => P a

printfA :: PrintfType t => String -> [P] -> t
-- need to reverse incoming list of args due to destructuring order below
printfA fmt args = printfA' fmt $ reverse args
  where printfA' :: PrintfType t => String -> [P] -> t
        printfA' _ [] = printf fmt
        -- this creates an expression like: printf fmt x x x where
        -- the x(s) are then applied to printf like multiple args
        printfA' _ (P x:xs) = (printfA' fmt xs) x

cmds :: String -> [VM] -> SymbolTableState String
cmds fStr elms = do
  -- get state in order to evaluate the genVM call so a
  -- string can be produced for printf, then wrap this
  -- value back up in the state and pass it along
  s <- S.get
  return $ printfA fStr $ map (P . (\(VM e) -> S.evalState (genVM e) s)) elms

evalExpr :: Expr -> SymbolTableState String
evalExpr (Expr expr) = do
  s <- S.get
  return $ S.evalState (postOrderExpr expr) s

evalExprs :: [Expr] -> SymbolTableState String
evalExprs exprs = do
  s <- S.get
  return $ concat $ S.evalState (mapM genVM exprs) s

lookupSym :: VarName -> SymbolTableState (Maybe SymbolInfo)
lookupSym vn = do
  s <- S.get
  return $ checkVal vn s
  where checkVal v (clsSyms, localSyms) =
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

instance VMGen Term where
  genVM (IntegerConstant i) = return $ printf "push %d\n" i
  genVM (StringConstant s) = return $ printf "push %s\n" s
  genVM (KeywordConstant k) = return $ printf "push %s\n" k
  genVM (VarName vn) = cmds "push %s " [VM vn]
  genVM (UnaryOp op t) = cmds "%s\n %s\n" [VM t, VM op]
  genVM (VarNameExpr vn expr) = do
    e <- evalExpr expr
    call <- cmds "call %s\n" [VM vn]
    return $ e <> call
  genVM (ParenExpr expr) = evalExpr expr
  genVM (SubroutineCall (SubCallName sn exprs)) = do
    es <- evalExprs exprs
    call <- cmds "call %s\n" [VM sn]
    return $ es <> call
  genVM (SubroutineCall (SubCallClassOrVar cvn sn exprs)) = do
    es <- evalExprs exprs
    call <- cmds "call %s.%s" [VM cvn, VM sn]
    return $ es <> call
  genVM (Op s) = cmds "%s\n" [VM s]

postOrderExpr :: Tree Term -> SymbolTableState String
postOrderExpr (Leaf t) = genVM t
postOrderExpr (Node lb op rb) = do
  s <- S.get
  lb' <- evalBranch lb s
  rb' <- evalBranch rb s
  op' <- (genVM op)
  return $ lb' <> rb' <> op'
  where evalBranch b s = do
          return $ (S.evalState (postOrderExpr b) s)

instance VMGen Expr where
  genVM (Expr expr) = do
    s <- S.get
    return $ S.evalState (postOrderExpr expr) s

instance VMGen [Expr] where
  -- TODO: double check if you need to add \n between the lists here
  genVM exprs = do
    s <- S.get
    return $ concat $ S.evalState (mapM genVM exprs) s
