{-# LANGUAGE FlexibleInstances #-}
module CodeGen where

import Text.Printf

import AST

class VMGen a where
  genVM :: a -> String

instance VMGen Symbol where
  genVM (Symbol s) = s

instance VMGen Keyword where
  genVM (Keyword k) = k

instance VMGen Identifier where
  genVM (Identifier i) = i

instance VMGen Term where
  genVM (IntegerConstant i) = printf "push %d\n" i
  genVM (StringConstant s) = printf "push %s\n" s
  genVM (KeywordConstant k) = printf "push %s\n" k
  genVM (VarName vn) = printf "push %s\n" (genVM vn)
  genVM (UnaryOp op t) = printf "%s\n %s\n" (genVM t) (genVM op)
  genVM (VarNameExpr vn (Expr expr)) =
    postOrderExpr expr ++ printf "call %s\n" (genVM vn)
  genVM (ParenExpr (Expr expr)) = postOrderExpr expr
  genVM (SubroutineCall (SubCallName sn exprs)) =
    (concatMap genVM exprs)
    ++ printf "call %s\n" (genVM sn)
  genVM (SubroutineCall (SubCallClassOrVar cvn sn exprs)) =
    (concatMap genVM exprs)
    ++ printf "call %s.%s" (genVM cvn) (genVM sn)
  genVM (Op s) = printf "%s\n" (genVM s)

unWrapExpr :: Expr -> Tree Term
unWrapExpr (Expr expr) = expr

postOrderExpr :: Tree Term -> String
postOrderExpr (Leaf t) = genVM t
postOrderExpr (Node lb op rb) =
  (postOrderExpr lb) <> (postOrderExpr rb) <> genVM op

instance VMGen Expr where
  genVM (Expr expr) = postOrderExpr expr

instance VMGen [Expr] where
  -- TODO: double check if you need to add \n between the lists here
  genVM exprs = concatMap genVM exprs
