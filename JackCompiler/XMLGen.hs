{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module XMLGen where

import Text.Printf
import Data.List

import AST

{-
 wrapper type using existential type to allow heterogenous lists.
 It's a trick to hide the type param from the left side of the
 data declartion so you can have a list of [WrapX] even though
 the types contained within each WrapX are different. Each type
 inside a WrapX must be an instance of XML and implement genXML.
-}
data WrapX = forall a . (XML a) => WrapX a

instance XML WrapX where
  genXML (WrapX a) = genXML a

genXTags :: [WrapX] -> String
genXTags [] =""
genXTags ((WrapX x):xs) = genXML x ++ genXTags xs

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

class XML a where
  genXML :: a -> String

instance XML Symbol where
  genXML (Symbol s) = xTag "symbol" s

instance XML Keyword where
  genXML (Keyword k) = xTag "keyword" k

instance XML Identifier where
  genXML (Identifier i) = xTag "identifier" i

instance XML Expr where
  genXML (Expr exprTree) = xTagMultiLine "expression"
    $ treeToStr
    $ fmap genXML exprTree
    where treeToStr (Leaf v) = v
          treeToStr (Node lb op rb) = (treeToStr lb) ++ op ++ (treeToStr rb)

instance XML [Expr] where
  genXML exprs = xTagMultiLine "expressionList"
                 $ intercalate (genXML (Symbol ",")) $ map genXML exprs

instance XML SubCall where
  genXML (SubCallName sName lp exprs rp) =
    genXTags [WrapX sName, WrapX lp, WrapX exprs, WrapX rp]
  genXML (SubCallClassOrVar cOrV d sName lp exprs rp) =
    genXTags [WrapX cOrV, WrapX d, WrapX sName, WrapX lp,
              WrapX exprs, WrapX rp]

termTag :: String -> String
termTag t = printf "<term>\n %s </term>\n" t

instance XML Term where
  genXML (IntegerConstant i) = termTag $ xTag "integerConstant" $ show i
  genXML (StringConstant s) = termTag $ xTag "stringConstant" s
  -- the grader expects "keyword" even though the grammar has "keywordConstant"
  -- as the type
  genXML (KeywordConstant k) = termTag $ xTag "keyword" k
  genXML (VarName i) = termTag $ genXML i -- VarName becomes an identifier tag
  genXML  (UnaryOp s t) = termTag $ genXML s ++ genXML t
  genXML (VarNameExpr vName expr) =
    termTag $ genXTags [WrapX vName,
                        WrapX (Symbol "["),
                        WrapX expr,
                        WrapX (Symbol "]")]
  genXML (ParenExpr lp expr rp) = termTag
    $ genXTags [WrapX lp, WrapX expr, WrapX rp]
  genXML (SubroutineCall subCall) = termTag $ genXML subCall
  genXML (Op s) = genXML s

instance XML LetVarName where
  genXML (LetVarName vn) = genXML vn
  genXML (LetVarNameExpr vn lb expr rb) =
    genXTags [WrapX vn, WrapX lb, WrapX expr, WrapX rb]

instance XML Else where
  genXML (Else kw lc stmts rc) =
    genXTags [WrapX kw, WrapX lc, WrapX stmts, WrapX rc]

instance XML [Statement] where
  genXML stmts = xTagMultiLine "statements" $ concatMap genXML stmts

instance XML Statement where
  genXML (Let kw vn eq expr sc) =
    xTagMultiLine "letStatement" $
    genXTags [WrapX kw, WrapX vn, WrapX eq, WrapX expr, WrapX sc]

  genXML (If kw lp expr rp lc stmts rc maybeStmts) =
    let mStmts = case maybeStmts of
          Just e -> genXML e
          Nothing -> ""
    in xTagMultiLine "ifStatement" $
    genXTags [WrapX kw, WrapX lp, WrapX expr, WrapX rp, WrapX lc,
              WrapX stmts, WrapX rc]
    ++ mStmts

  genXML (While kw lp expr rp lc stmts rc) =
    xTagMultiLine "whileStatement" $
    genXTags [WrapX kw, WrapX lp, WrapX expr, WrapX rp, WrapX lc,
              WrapX stmts, WrapX rc]

  genXML (Do kw subCall sc) =
    xTagMultiLine "doStatement" $
    genXTags [WrapX kw, WrapX subCall, WrapX sc]

  genXML (Return kw maybeExpr sc) =
    let mExpr = case maybeExpr of
          (Just expr) -> genXML expr
          Nothing -> ""
    in xTagMultiLine "returnStatement" $
    genXML kw
    ++ mExpr
    ++ genXML sc

instance XML Type where
  genXML (TKeyword kw) = genXML kw
  genXML (TIdentifier i) = genXML i

instance XML VarDec where
  genXML (VarDec varKw typ vn vns sc) =
    let varNames = intercalate (genXML (Symbol ",")) $
                   map genXML $ [vn] ++ vns
    in xTagMultiLine "varDec" $
       genXTags [WrapX varKw, WrapX typ]
       ++ varNames
       ++ genXML sc

instance XML [VarDec] where
  genXML varDecs = concatMap genXML varDecs

instance XML SubroutineBody where
  genXML (SubroutineBody lc varDecs stmts rc) =
    xTagMultiLine "subroutineBody" $
    genXTags [WrapX lc, WrapX varDecs, WrapX stmts, WrapX rc]

instance XML ParameterList where
  genXML (ParameterList params) =
    xTagMultiLine "parameterList" $
    intercalate (genXML (Symbol ",")) $ map genParamsXML params
    where genParamsXML (t, vn) = genXTags [WrapX t, WrapX vn]

instance XML SubroutineDec where
  genXML (SubroutineDec kw typ sn lp pList rp sb) =
    xTagMultiLine "subroutineDec" $
    genXTags [WrapX kw, WrapX typ, WrapX sn, WrapX lp, WrapX pList,
              WrapX rp, WrapX sb]

instance XML [SubroutineDec] where
  genXML subDecs = concatMap genXML subDecs

instance XML ClassVarDec where
  genXML (ClassVarDec kw typ vn vns sc) =
    let varNames = intercalate (genXML (Symbol ",")) $
                   map genXML $ [vn] ++ vns
    in xTagMultiLine "classVarDec" $
    genXTags [WrapX kw, WrapX typ]
    ++ varNames
    ++ genXML sc

instance XML [ClassVarDec] where
  genXML cvDecs = concatMap genXML cvDecs

instance XML Class where
  genXML (Class kw cn lc clsVars subDecs rc) =
    xTagMultiLine "class" $
    genXTags [WrapX kw, WrapX cn, WrapX lc, WrapX clsVars,
              WrapX subDecs, WrapX rc]
