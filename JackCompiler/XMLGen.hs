module XMLGen where

import Text.Printf
import Data.List

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
xExprs exprs = intercalate (xSymbol (Symbol ",")) $ map xExpr exprs

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
-- TODO: might not need to wrap xIdentifier in term tag
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

xStatements :: [Statement] -> String
xStatements stmts = concat $ map xStatement stmts

xStatement :: Statement -> String
xStatement (Let kw varName eq expr sc) =
  let varN = case varName of
        LetVarName vn ->
          xIdentifier vn
        LetVarNameExpr vn lb expr' rb ->
          xIdentifier vn
          ++ xSymbol lb
          ++ xExpr expr'
          ++ xSymbol rb
  in xTag "letStatement" $
  xKeyword kw
  ++ varN
  ++ xSymbol eq
  ++ xExpr expr
  ++ xSymbol sc

xStatement (If kw lp expr rp lc stmts rc maybeStmts) =
  let mStmts = case maybeStmts of
        Just (Else kw' lc' stmts' rc') ->
          xKeyword kw'
          ++ xSymbol lc'
          ++ xStatements stmts'
          ++ xSymbol rc'
        Nothing -> ""
  in xTag "ifStatement" $
  xKeyword kw
  ++ xSymbol lp
  ++ xExpr expr
  ++ xSymbol rp
  ++ xSymbol lc
  ++ xStatements stmts
  ++ xSymbol rc
  ++ mStmts

xStatement (While kw lp expr rp lc stmts rc) =
  xTag "whileStatement" $
  xKeyword kw
  ++ xSymbol lp
  ++ xExpr expr
  ++ xSymbol rp
  ++ xSymbol lc
  ++ xStatements stmts
  ++ xSymbol rc

xStatement (Do kw subCall) =
  let sCall = case subCall of
        (SubCallName scn lp exprs rp) ->
          xIdentifier scn
          ++ xSymbol lp
          ++ xExprs exprs
          ++ xSymbol rp
        (SubCallClassOrVar cvn dot sn lp exprs rp) ->
          xIdentifier cvn
          ++ xSymbol dot
          ++ xIdentifier sn
          ++ xSymbol lp
          ++ xExprs exprs
          ++ xSymbol rp
  in xTag "doStatement" $
  xKeyword kw
  ++ sCall

xStatement (Return kw maybeExpr sc) =
  let mExpr = case maybeExpr of
        (Just expr) -> xExpr expr
        Nothing -> ""
  in xTag "returnStatement" $
  xKeyword kw
  ++ mExpr
  ++ xSymbol sc

xType :: Type -> String
xType (TKeyword kw) = xKeyword kw
xType (TIdentifier i) = xIdentifier i

xVarDec :: VarDec -> String
xVarDec (VarDec varKw typ vn vns sc) =
  let varNames = intercalate (xSymbol (Symbol ",")) $
                 map xIdentifier $ [vn] ++ vns
  in xKeyword varKw
  ++ xType typ
  ++ varNames
  ++ xSymbol sc

xSubroutineBody :: SubroutineBody -> String
xSubroutineBody (SubroutineBody lc varDecs stmts rc) =
  xSymbol lc
  ++ (concat $ map xVarDec varDecs)
  ++ xStatements stmts
  ++ xSymbol rc
