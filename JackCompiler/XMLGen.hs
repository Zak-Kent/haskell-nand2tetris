module XMLGen where

import Text.Printf
import AST

xTag :: String -> String -> String
xTag name val = printf "<%s>%s</%s>" name val name

xSymbol :: Symbol -> String
xSymbol (Symbol s) = xTag "symbol" s

xKeyword :: Keyword -> String
xKeyword (Keyword k) = xTag "keyword" k

xIdentifier :: Identifier -> String
xIdentifier (Identifier i) = xTag "identifier" i

xOpTerms :: (Op, Term) -> String
xOpTerms ((Op o), t) = xSymbol o ++ xTerm t

xExpr :: Expr -> String
xExpr (Expr t opTerms) = xTag "expression"
  (concat $ [xTerm t] ++ (map xOpTerms opTerms))

xExprs :: [Expr] -> String
xExprs exprs = concat $ map xExpr exprs

xSubCall :: SubCall -> String
xSubCall (SubCallName sName lp exprs rp) =
  xIdentifier sName
  ++ xSymbol lp
  ++ xExprs exprs
  ++ xSymbol rp
xSubCall (SubCallClassOrVar cOrVName dot sName lp exprs rp) =
  xIdentifier cOrVName
  ++ xSymbol dot
  ++ xIdentifier sName
  ++ xSymbol lp
  ++ xExprs exprs
  ++ xSymbol rp

xWrapT :: String -> String
xWrapT t = printf "<term>%s</term>" t

xTerm :: Term -> String
xTerm (IntegerConstant i) = xWrapT $ xTag "integerConstant" $ show i
xTerm (StringConstant s) = xWrapT $ xTag "stringConstant" s
xTerm (KeywordConstant k) = xWrapT $ xTag "keywordConstant" k
xTerm (VarName i) = xWrapT $ xIdentifier i -- VarName becomes an identifier tag
xTerm (UnaryOp s t) = xWrapT $ xSymbol s ++ xTerm t
xTerm (VarNameExpr vName lb expr rb) =
  xWrapT $
  xIdentifier vName
  ++ xSymbol lb
  ++ xExpr expr
  ++ xSymbol rb
xTerm (ParenExpr lp expr rp) = xWrapT $ xSymbol lp ++ xExpr expr ++ xSymbol rp
xTerm (SubroutineCall subCall) = xWrapT $ xSubCall subCall

