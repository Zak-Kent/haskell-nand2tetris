module XMLGen where

import Text.Printf
import Data.List

import AST

xEscape :: String -> String
xEscape "<"  = "&lt;"
xEscape ">"  = "&gt;"
xEscape "&"  = "&amp;"
xEscape "\"" = "&quot;"
xEscape x    = x

xTag :: String -> String -> String
xTag name val = printf "<%s> %s </%s>\n" name (xEscape val) name

xTagMultiLine :: String -> String -> String
xTagMultiLine name val = printf "<%s>\n %s </%s>\n" name val name

xSymbol :: Symbol -> String
xSymbol (Symbol s) = xTag "symbol" s

xKeyword :: Keyword -> String
xKeyword (Keyword k) = xTag "keyword" k

xIdentifier :: Identifier -> String
xIdentifier (Identifier i) = xTag "identifier" i

xOpTerms :: (Op, Term) -> String
xOpTerms ((Op o), t) = xSymbol o ++ xTerm t

xExpr :: Expr -> String
xExpr (Expr t opTerms) = xTagMultiLine "expression"
  (concat $ [xTerm t] ++ (map xOpTerms opTerms))

xExprs :: [Expr] -> String
xExprs exprs = xTagMultiLine "expressionList"
               $ intercalate (xSymbol (Symbol ",")) $ map xExpr exprs

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
xWrapT t = printf "<term>\n %s </term>\n" t

xTerm :: Term -> String
xTerm (IntegerConstant i) = xWrapT $ xTag "integerConstant" $ show i
xTerm (StringConstant s) = xWrapT $ xTag "stringConstant" s
-- the grader expects "keyword" even though the grammar has "keywordConstant"
-- as the type
xTerm (KeywordConstant k) = xWrapT $ xTag "keyword" k
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
xStatements stmts = xTagMultiLine "statements" $ concat $ map xStatement stmts

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
  in xTagMultiLine "letStatement" $
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
  in xTagMultiLine "ifStatement" $
  xKeyword kw
  ++ xSymbol lp
  ++ xExpr expr
  ++ xSymbol rp
  ++ xSymbol lc
  ++ xStatements stmts
  ++ xSymbol rc
  ++ mStmts

xStatement (While kw lp expr rp lc stmts rc) =
  xTagMultiLine "whileStatement" $
  xKeyword kw
  ++ xSymbol lp
  ++ xExpr expr
  ++ xSymbol rp
  ++ xSymbol lc
  ++ xStatements stmts
  ++ xSymbol rc

xStatement (Do kw subCall sc) =
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
  in xTagMultiLine "doStatement" $
  xKeyword kw
  ++ sCall
  ++ xSymbol sc

xStatement (Return kw maybeExpr sc) =
  let mExpr = case maybeExpr of
        (Just expr) -> xExpr expr
        Nothing -> ""
  in xTagMultiLine "returnStatement" $
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
  in xTagMultiLine "varDec" $
     xKeyword varKw
     ++ xType typ
     ++ varNames
     ++ xSymbol sc

xSubroutineBody :: SubroutineBody -> String
xSubroutineBody (SubroutineBody lc varDecs stmts rc) =
  xTagMultiLine "subroutineBody" $
  xSymbol lc
  ++ (concat $ map xVarDec varDecs)
  ++ xStatements stmts
  ++ xSymbol rc

xParameterList :: ParameterList -> String
xParameterList (ParameterList params) =
  xTagMultiLine "parameterList" $
  intercalate (xSymbol (Symbol ",")) $ map xParams params
  where xParams (t, vn) = xType t ++ xIdentifier vn

xSubroutineDec :: SubroutineDec -> String
xSubroutineDec (SubroutineDec kw typ sn lp pList rp sb) =
  xTagMultiLine "subroutineDec" $
  xKeyword kw
  ++ xType typ
  ++ xIdentifier sn
  ++ xSymbol lp
  ++ xParameterList pList -- this produces an empty tag if no params, could be a bug
  ++ xSymbol rp
  ++ xSubroutineBody sb

xClassVarDec :: ClassVarDec -> String
xClassVarDec (ClassVarDec kw typ vn vns sc) =
  let varNames = intercalate (xSymbol (Symbol ",")) $
                 map xIdentifier $ [vn] ++ vns
  in xTagMultiLine "classVarDec" $
  xKeyword kw
  ++ xType typ
  ++ varNames
  ++ xSymbol sc

xClass :: Class -> String
xClass (Class kw cn lc clsVars subDecs rc) =
  xTagMultiLine "class" $
  xKeyword kw
  ++ xIdentifier cn
  ++ xSymbol lc
  ++ (concat $ map xClassVarDec clsVars)
  ++ (concat $ map xSubroutineDec subDecs)
  ++ xSymbol rc
