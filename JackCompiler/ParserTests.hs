module ParserTests where

import Data.Either
import Test.HUnit
import qualified Text.Parsec as Ps
import qualified Text.ParserCombinators.Parsec.Error as PsE
import qualified Text.Parsec.Pos as Pos

import AST
import Parser

parseIt :: (Ps.Parsec String () a) -> String ->  Either Ps.ParseError a
parseIt p s = Ps.parse p "test error" s

buildParseError :: String -> String -> (Int, Int) -> Ps.ParseError
buildParseError m sn (l, c) = PsE.newErrorMessage
                            (PsE.Message m)
                            (Pos.newPos sn l c)

getErrorP :: (Ps.Parsec String () a) -> String -> [PsE.Message]
getErrorP p s = PsE.errorMessages $ fromLeft placeHolderErr err
  where err = parseIt p s
        placeHolderErr = buildParseError "default msg" "default src" (1, 1)

-- How to build a Ps.ParseError if you need to check error messages:
-- you'll need a wrapper func to return a Either Ps.ParseError Term
-- tKeywordPFail = TestCase (assertEqual "keyword failure"
--                                   (Left
--                                     (buildParseError "unexpected \"x\"\nexpecting \"-\" or \"~\""
--                                          "test error" (1,1)))
--                                   $ parseIt unaryOpP "x")

-- Terminal element parser tests
{-
  These terminal element tests aren't comprehensive but all parsers
  of this type use the same chooseLit helper func. The tests below
  assert that these types of parsers can handle arbitrary whitespace
  and fail when the don't recognize an input.
-}
tKeywordP = TestCase (assertEqual "keyword parse"
                       (Right (Keyword "class"))
                       $ parseIt (keywordP "class") "   class   ")

tKeywordPFail = TestCase (assertBool "keywordP fail"
                           (isLeft $ parseIt (keywordP "class") "x"))

tSymbolP = TestCase (assertEqual "symbol parse"
                      (Right (Symbol "+"))
                      $ parseIt (symP "+") "  +  ")

-- identifierP tests
tIdentifierP = TestCase (assertEqual "identifier parse"
                          (Right (Identifier "foo123"))
                          $ parseIt identifierP "foo123")

tIdentifierPSpaces = TestCase (assertEqual "identifierP handles spaces"
                              (Right (Identifier "bar"))
                              $ parseIt identifierP "     bar     ")

tIdentifierPNoDigit =
  TestCase (assertBool "identifierP doesn't allow a digit in first pos"
           (isLeft $ parseIt identifierP "1foo"))

-- exprP tests
tExprOneTerm = TestCase (assertEqual "an expression with one term parses"
                        (Right (Expr (Leaf (IntegerConstant 5))))
                        $ parseIt exprP "5")

tExprTwoTerms = TestCase (assertEqual "an expression with two terms parses"
                        (Right (Expr (Node (Leaf (IntegerConstant 5))
                                       (Op (Symbol "+"))
                                       (Leaf (IntegerConstant 8)))))
                        $ parseIt exprP "5 + 8")

tExprManyTerms = TestCase (assertEqual "an expression with many terms parses"
                          (Right
                           (Expr
                            (Node (Leaf (VarName (Identifier "foo")))
                             (Op (Symbol "="))
                             (Node (Leaf (IntegerConstant 4))
                              (Op (Symbol "+"))
                              (Leaf (IntegerConstant 2))))))
                        $ parseIt exprP "foo = 4 + 2")

-- subCallNameP tests
tBasicSubCall = TestCase (assertEqual "a sub routine with expr list parses"
                         (Right
                          (SubCallName (Identifier "foo")
                            [Expr
                              (Node
                                (Leaf (IntegerConstant 5))
                                (Op (Symbol "+"))
                                (Leaf (IntegerConstant 8))),
                              Expr
                              (Node
                                (Leaf (IntegerConstant 10))
                                (Op (Symbol "-"))
                                (Leaf (IntegerConstant 42)))]))
                           $ parseIt subCallNameP "foo (5+8, 10  - 42)")

tSubCallNoExprs = TestCase (assertEqual "a sub routine with no exprs"
                            (Right (SubCallName (Identifier "bar") []))
                            $ parseIt subCallNameP "bar()    ")

-- subCallClassOrVarP tests
tSubCallClassWithExprs = TestCase (assertEqual "a class sub routine with exprs"
                                  (Right (SubCallClassOrVar (Identifier "foo")
                                         (Identifier "bar")
                                         [(Expr
                                           (Node
                                            (Leaf (VarName (Identifier "bb")))
                                            (Op (Symbol "="))
                                            (Leaf (IntegerConstant 5)))),
                                           (Expr
                                            (Node
                                             (Leaf (IntegerConstant 10))
                                             (Op (Symbol "-"))
                                             (Leaf (IntegerConstant 42))))]))
                                  $ parseIt
                                    subCallClassOrVarP "foo.bar(bb = 5, 10-42)")

tSubCallClassNoExprs = TestCase (assertEqual "a class sub routine with exprs"
                                  (Right (SubCallClassOrVar (Identifier "biz")
                                          (Identifier "baz") []))
                                  $ parseIt
                                    subCallClassOrVarP "  biz  .  baz  ()")

-- subroutineCallP tests
tClassSubroutineCall = TestCase (assertEqual "class sub routine"
                                  (Right
                                    (SubroutineCall
                                     (SubCallClassOrVar (Identifier "what")
                                      (Identifier "huh") [])))
                                  $ parseIt subroutineCallP "what.huh()")

tSubroutineCall = TestCase (assertEqual "sub routine name"
                             (Right
                               (SubroutineCall
                                 (SubCallName (Identifier "things")
                                   [(Expr
                                      (Node
                                        (Leaf (IntegerConstant 1))
                                        (Op (Symbol "+"))
                                        (Leaf (IntegerConstant 2))))])))
                                 $ parseIt subroutineCallP "things(1+2)")

-- varNameExprP tests
tVarNameArrayAcccess = TestCase (assertEqual "array access using []"
                                  (Right
                                    (VarNameExpr (Identifier "foo")
                                      (Expr
                                       (Node
                                        (Leaf (IntegerConstant 1))
                                        (Op (Symbol "+"))
                                        (Leaf (IntegerConstant 2))))))
                                  $ parseIt varNameExprP "foo[1 + 2]")

-- parenExprP tests
tParensWrappingExpression = TestCase (assertEqual "parens wrapping expression"
                                       (Right
                                         (ParenExpr
                                           (Expr
                                            (Node
                                             (Leaf (IntegerConstant 1))
                                             (Op (Symbol "+"))
                                             (Node
                                              (Leaf (IntegerConstant 2))
                                              (Op (Symbol "+"))
                                              (Node
                                               (Leaf (IntegerConstant 3))
                                               (Op (Symbol "+"))
                                               (Leaf (IntegerConstant 4))))))))
                                       $ parseIt parenExprP "(1+2 +   3  + 4)")

-- termP tests
tTermArrayAccess = TestCase (assertEqual "term array access: foo[1]"
                              (Right
                               (VarNameExpr (Identifier "foo")
                                 (Expr (Leaf (IntegerConstant 1)))))
                              $ parseIt termP "foo[1]")

tTermMethodCall = TestCase (assertEqual "method call foo.bar()"
                             (Right
                               (SubroutineCall
                                 (SubCallClassOrVar (Identifier "foo")
                                   (Identifier "bar") [])))
                           $ parseIt termP "foo.bar()")

tTermSubroutineCall = TestCase (assertEqual "subroutine call baz()"
                                 (Right
                                   (SubroutineCall
                                     (SubCallName (Identifier "baz") [])))
                                 $ parseIt termP "baz()")

tTermVarName = TestCase (assertEqual "varname blarg"
                          (Right (VarName (Identifier "blarg")))
                          $ parseIt termP "blarg")

-- statementP tests
tLetStatement = TestCase (assertEqual "let statement with simple varname"
                           (Right
                             (Let
                               (LetVarName (Identifier "foo"))
                               (Expr (Leaf (IntegerConstant 5)))))
                         $ parseIt letP "let foo = 5;")

tLetStatementWExpr = TestCase (assertEqual "let statement with expr varname"
                                (Right
                                  (Let
                                    (LetVarNameExpr (Identifier "foo")
                                      (Expr (Leaf (IntegerConstant 1))))
                                    (Expr (Leaf (IntegerConstant 5)))))
                         $ parseIt letP "let foo[1] = 5;")

tIfStatementNoElse = TestCase (assertEqual "if statement no else"
                                (Right
                                  (If
                                    (Expr
                                     (Node
                                      (Leaf (IntegerConstant 1))
                                      (Op (Symbol "+"))
                                      (Node
                                       (Leaf (IntegerConstant 2))
                                       (Op (Symbol "="))
                                       (Leaf (IntegerConstant 3)))))
                                    [Let
                                      (LetVarName (Identifier "foo"))
                                      (Expr (Leaf (IntegerConstant 6)))]
                                    Nothing))
                                $ parseIt ifP "if (1 + 2 = 3) {let foo = 6;}")

tIfStatementElse = TestCase (assertEqual "if statement with else"
                              (Right
                                (If
                                  (Expr
                                     (Node
                                      (Leaf (IntegerConstant 1))
                                      (Op (Symbol "+"))
                                      (Node
                                       (Leaf (IntegerConstant 2))
                                       (Op (Symbol "="))
                                       (Leaf (IntegerConstant 3)))))
                                  [Let
                                    (LetVarName (Identifier "foo"))
                                    (Expr (Leaf (IntegerConstant 6)))]
                                  (Just
                                    (Else
                                      [Let
                                        (LetVarName (Identifier "foo"))
                                        (Expr (Leaf (IntegerConstant 2)))]))))
                              $ parseIt ifP
                              "if ( 1 + 2 = 3) {let foo = 6;} else {let foo = 2;}")

tWhileStatement = TestCase (assertEqual "while block"
                             (Right
                               (While
                                 (Expr
                                  (Node
                                   (Leaf (VarName (Identifier "foo")))
                                   (Op (Symbol "="))
                                   (Leaf (KeywordConstant "true"))))
                                 [Let
                                   (LetVarName (Identifier "bar"))
                                   (Expr (Leaf (IntegerConstant 7)))]))
                             $ parseIt whileP "while (foo = true) {let bar = 7;}")

tDoSubCallStatement = TestCase (assertEqual "do what.huh()"
                                 (Right
                                   (Do
                                     (SubCallClassOrVar (Identifier "what")
                                      (Identifier "huh") [])))
                                 $ parseIt doP "do what.huh() ;")

tReturnStatementWithExpr = TestCase (assertEqual "return 1 + 2;"
                                      (Right
                                        (Return
                                          (Just
                                           (Expr
                                            (Node
                                             (Leaf (IntegerConstant 1))
                                             (Op (Symbol "+"))
                                             (Leaf (IntegerConstant 2)))))))
                                      $ parseIt returnP "return 1 +     2;")

tReturnNoExpr = TestCase (assertEqual "return;"
                           (Right
                             (Return Nothing))
                         $ parseIt returnP "return;")

-- Program structure tests
tVarDecOneVar = TestCase (assertEqual "var int foo;"
                           (Right
                             (VarDec
                               (TKeyword (Keyword "int"))
                               (Identifier "foo") []))
                           $ parseIt varDecP "var int foo;")

tVarDecManyVars = TestCase (assertEqual "var boolean foo, bar, baz;"
                             (Right
                               (VarDec
                               (TKeyword (Keyword "boolean"))
                               (Identifier "foo")
                               [(Identifier "bar"),
                                (Identifier "baz")]))
                           $ parseIt varDecP "var boolean foo   , bar, baz;")

tSubroutineBody = TestCase (assertEqual "a simple subroutine body"
                             (Right
                               (SubroutineBody
                                 [VarDec
                                   (TKeyword (Keyword "int"))
                                   (Identifier "foo") [] ,
                                  VarDec
                                   (TKeyword (Keyword "boolean"))
                                   (Identifier "bar") []]
                                 [Let
                                   (LetVarName (Identifier "foo"))
                                   (Expr (Leaf (IntegerConstant 5))) ,
                                  Let
                                   (LetVarName (Identifier "bar"))
                                   (Expr (Leaf (KeywordConstant "true")))]))
                             $ parseIt subroutineBodyP
                             "{var int foo; var boolean bar; let foo = 5; let bar = true;}")

tSubroutineBodyNoVars = TestCase (assertEqual "subroutine body with no vars"
                                  (Right
                                    (SubroutineBody
                                      []
                                      [(Do
                                         (SubCallClassOrVar (Identifier "what")
                                           (Identifier "huh") []))]))
                                 $ parseIt subroutineBodyP
                                 "{   do what.huh(); }")

tParamListOneParam = TestCase (assertEqual "param list with one param"
                                (Right
                                  (ParameterList
                                    [(TKeyword (Keyword "int"),
                                      Identifier "foo")]))
                                $ parseIt paramListP "int foo")

tParamListManyParams = TestCase (assertEqual "param list with one param"
                                  (Right
                                  (ParameterList
                                    [(TKeyword (Keyword "int"),
                                      Identifier "foo"),
                                     (TKeyword (Keyword "boolean"),
                                      Identifier "bar"),
                                     (TIdentifier (Identifier "classFoo"),
                                      Identifier "baz")]))
                                $ parseIt paramListP "int foo, boolean bar, classFoo baz")

tSubroutineDec = TestCase (assertEqual "subroutine declaration"
                            (Right
                              (SubroutineDec (Keyword "method")
                                (TKeyword (Keyword "void"))
                                (Identifier "foo")
                                (ParameterList
                                  [(TKeyword (Keyword "int"),
                                    Identifier "arg1"),
                                    (TKeyword (Keyword "boolean"),
                                     Identifier "arg2")])
                                (SubroutineBody
                                  [VarDec
                                    (TKeyword (Keyword "int"))
                                    (Identifier "baz") []]
                                  [Let
                                    (LetVarName (Identifier "baz"))
                                    (Expr
                                     (Node
                                      (Leaf (VarName (Identifier "arg1")))
                                      (Op (Symbol "+"))
                                      (Leaf (IntegerConstant 5)))) ,
                                    Return Nothing])))
                          $ parseIt subroutineDecP
                          "method void foo (int arg1, boolean arg2) \
                          \ { var int baz; let baz = arg1 + 5; return;}")


tClassVarDecManyVars = TestCase (assertEqual "var boolean foo, bar, baz;"
                                  (Right
                                    (ClassVarDec (Keyword "static")
                                      (TKeyword (Keyword "int"))
                                      (Identifier "foo")
                                      [(Identifier "bar")]))
                           $ parseIt classVarDecP "static int foo, bar;")

tClassWithBasicElems = TestCase (assertEqual "a basic class"
                                 (Right
                                  (Class (Keyword "class")
                                    (Identifier "foo")
                                    [ClassVarDec
                                      (Keyword "static")
                                      (TKeyword (Keyword "int"))
                                      (Identifier "foo")
                                      [Identifier "bar"] ,
                                     ClassVarDec
                                      (Keyword "static")
                                      (TKeyword (Keyword "boolean"))
                                      (Identifier "baz")
                                      [Identifier "buz"]]
                                    [SubroutineDec (Keyword "method")
                                      (TKeyword (Keyword "void"))
                                      (Identifier "doFoo")
                                      (ParameterList
                                        [(TKeyword (Keyword "int"),
                                          Identifier "foo"),
                                          (TKeyword (Keyword "boolean"),
                                           Identifier "baz")])
                                      (SubroutineBody
                                        []
                                        [Return
                                          (Just
                                            (Expr
                                             (Leaf (VarName (Identifier "foo")))))])]))
                           $ parseIt classP "class foo { static int foo, bar; \
                                           \ static boolean baz, buz; \
                                           \ method void doFoo (int foo, boolean baz) \
                                           \ { return foo; }}")


-- TestLists
terminalParserTests =
  TestList [TestLabel "keywordP" tKeywordP,
            TestLabel "keywordP fail" tKeywordPFail,
            TestLabel "symbolP" tSymbolP]

identifierPTests =
  TestList [TestLabel "identifierP" tIdentifierP,
            TestLabel "identifierP spaces" tIdentifierPSpaces,
            TestLabel "identifierP no digit" tIdentifierPNoDigit]

exprPTests =
  TestList [TestLabel "exprP one term" tExprOneTerm,
            TestLabel "exprP two term" tExprTwoTerms,
            TestLabel "exprP many terms" tExprManyTerms]

subroutineCallPTests =
  TestList [TestLabel "sub routine call with expr list" tBasicSubCall,
            TestLabel "sub rountne call with no exprs" tSubCallNoExprs,
            TestLabel "class sub routine with exprs" tSubCallClassWithExprs,
            TestLabel "class sub routine no exprs" tSubCallClassNoExprs,
            TestLabel "class subroutine call in termP" tClassSubroutineCall,
            TestLabel "subroutine call in termP" tSubroutineCall]

varNameExprPTests =
  TestList [TestLabel "array access foo[]" tVarNameArrayAcccess]

parenExprPTests =
  TestList [TestLabel "parens wrapping an expr" tParensWrappingExpression]

termPTests =
  TestList [TestLabel "foo[1]" tTermArrayAccess,
            TestLabel "foo.bar()" tTermMethodCall,
            TestLabel "baz()" tTermSubroutineCall,
            TestLabel "blarg" tTermVarName]

-- Statement tests
statmentTests =
  TestList [TestLabel "let foo = 5;" tLetStatement,
            TestLabel "let foo[1] = 5;" tLetStatementWExpr,
            TestLabel "if (1 + 2 = 3) {let foo = 6;}" tIfStatementNoElse,
            TestLabel "if ( 1 + 2 = 3) {let foo = 6;} else {let foo = 2;}"
              tIfStatementElse,
            TestLabel "while (foo = true) {let bar = 7;}" tWhileStatement,
            TestLabel "do what.huh()" tDoSubCallStatement,
            TestLabel "return 1 + 2;" tReturnStatementWithExpr,
            TestLabel "return;" tReturnNoExpr,
            TestLabel "class var dec two vars" tClassVarDecManyVars]

-- Program structure tests
structureTests =
  TestList [TestLabel "var int foo;" tVarDecOneVar,
            TestLabel "var boolean foo, bar, baz;" tVarDecManyVars,
            TestLabel "simple subroutine body" tSubroutineBody,
            TestLabel "subroutine body with no vars" tSubroutineBodyNoVars,
            TestLabel "param list with on param" tParamListOneParam,
            TestLabel "param list with many params" tParamListManyParams,
            TestLabel "subroutine declaration" tSubroutineDec,
            TestLabel "basic class" tClassWithBasicElems]

runAllTests :: Test
runAllTests =
  TestList
  $ concat $ [l | (TestList l) <- [terminalParserTests,
                                   identifierPTests,
                                   exprPTests,
                                   subroutineCallPTests,
                                   varNameExprPTests,
                                   parenExprPTests,
                                   termPTests,
                                   statmentTests,
                                   structureTests]]

-- run in REPL with: runTestTT (<TestList> | runAllTests)
