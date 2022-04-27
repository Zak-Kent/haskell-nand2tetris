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
                        (Right (Expr (IntegerConstant 5) []))
                        $ parseIt exprP "5")

tExprTwoTerms = TestCase (assertEqual "an expression with two terms parses"
                        (Right (Expr (IntegerConstant 5)
                                [(Op (Symbol "+"), (IntegerConstant 8))]))
                        $ parseIt exprP "5 + 8")

tExprManyTerms = TestCase (assertEqual "an expression with many terms parses"
                        (Right (Expr (VarName (Identifier "foo"))
                                [(Op (Symbol "="), (IntegerConstant 4)),
                                 (Op (Symbol "+"), (IntegerConstant 2))]))
                        $ parseIt exprP "foo = 4 + 2")

-- subCallNameP tests
tBasicSubCall = TestCase (assertEqual "a sub routine with expr list parses"
                         (Right (SubCallName (Identifier "foo")
                                (Symbol "(")
                                [(Expr (IntegerConstant 5)
                                  [(Op (Symbol "+"), (IntegerConstant 8))]),
                                  (Expr (IntegerConstant 10)
                                  [(Op (Symbol "-"), (IntegerConstant 42))])]
                                (Symbol ")")))
                           $ parseIt subCallNameP "foo (5+8, 10  - 42)")

tSubCallNoExprs = TestCase (assertEqual "a sub routine with no exprs"
                            (Right (SubCallName (Identifier "bar")
                                    (Symbol "(")
                                    []
                                    (Symbol ")")))
                            $ parseIt subCallNameP "bar()    ")

-- subCallClassOrVarP tests
tSubCallClassWithExprs = TestCase (assertEqual "a class sub routine with exprs"
                                  (Right (SubCallClassOrVar (Identifier "foo")
                                         (Symbol ".")
                                         (Identifier "bar")
                                         (Symbol "(")
                                         [(Expr (VarName (Identifier  "bb"))
                                           [(Op (Symbol "="), (IntegerConstant 5))]),
                                          (Expr (IntegerConstant 10)
                                           [(Op (Symbol "-"), (IntegerConstant 42))])]
                                         (Symbol ")")))
                                  $ parseIt
                                    subCallClassOrVarP "foo.bar(bb = 5, 10-42)")

tSubCallClassNoExprs = TestCase (assertEqual "a class sub routine with exprs"
                                  (Right (SubCallClassOrVar (Identifier "biz")
                                          (Symbol ".")
                                          (Identifier "baz")
                                          (Symbol "(")
                                          []
                                          (Symbol ")")))
                                  $ parseIt
                                    subCallClassOrVarP "  biz  .  baz  ()")

-- subroutineCallP tests
tClassSubroutineCall = TestCase (assertEqual "class sub routine"
                                  (Right
                                    (SubroutineCall
                                     (SubCallClassOrVar (Identifier "what")
                                      (Symbol ".")
                                      (Identifier "huh")
                                      (Symbol "(")
                                      []
                                      (Symbol ")"))))
                                  $ parseIt subroutineCallP "what.huh()")

tSubroutineCall = TestCase (assertEqual "sub routine name"
                             (Right
                               (SubroutineCall
                                 (SubCallName (Identifier "things")
                                   (Symbol "(")
                                   [(Expr (IntegerConstant 1)
                                     [(Op (Symbol "+"), (IntegerConstant 2))])]
                                   (Symbol ")"))))
                                 $ parseIt subroutineCallP "things(1+2)")

-- varNameExprP tests
tVarNameArrayAcccess = TestCase (assertEqual "array access using []"
                                  (Right
                                    (VarNameExpr (Identifier "foo")
                                      (Symbol "[")
                                      (Expr (IntegerConstant 1)
                                        [(Op (Symbol "+"), (IntegerConstant 2))])
                                      (Symbol "]")))
                                  $ parseIt varNameExprP "foo[1 + 2]")

-- parenExprP tests
tParensWrappingExpression = TestCase (assertEqual "parens wrapping expression"
                                       (Right
                                         (ParenExpr
                                           (Symbol "(")
                                           (Expr (IntegerConstant 1)
                                            [(Op (Symbol "+"), (IntegerConstant 2)),
                                             (Op (Symbol "+"), (IntegerConstant 3)),
                                             (Op (Symbol "+"), (IntegerConstant 4))])
                                           (Symbol ")")))
                                       $ parseIt parenExprP "(1+2 +   3  + 4)")

-- termP tests
tTermArrayAccess = TestCase (assertEqual "term array access: foo[1]"
                              (Right
                               (VarNameExpr (Identifier "foo")
                                 (Symbol "[")
                                 (Expr (IntegerConstant 1) [])
                                 (Symbol "]")))
                              $ parseIt termP "foo[1]")

tTermMethodCall = TestCase (assertEqual "method call foo.bar()"
                             (Right
                               (SubroutineCall
                                 (SubCallClassOrVar (Identifier "foo")
                                   (Symbol ".")
                                   (Identifier "bar")
                                   (Symbol "(")
                                   []
                                   (Symbol ")"))))
                           $ parseIt termP "foo.bar()")

tTermSubroutineCall = TestCase (assertEqual "subroutine call baz()"
                                 (Right
                                   (SubroutineCall
                                     (SubCallName (Identifier "baz")
                                       (Symbol "(")
                                       []
                                       (Symbol ")"))))
                                 $ parseIt termP "baz()")

tTermVarName = TestCase (assertEqual "varname blarg"
                          (Right (VarName (Identifier "blarg")))
                          $ parseIt termP "blarg")

-- statementP tests
tLetStatement = TestCase (assertEqual "let statement with simple varname"
                           (Right
                             (Let (Keyword "let")
                               (LetVarName (Identifier "foo"))
                               (Symbol "=")
                               (Expr (IntegerConstant 5) [])
                               (Symbol ";")))
                         $ parseIt letP "let foo = 5;")

tLetStatementWExpr = TestCase (assertEqual "let statement with expr varname"
                                (Right
                                  (Let (Keyword "let")
                                    (LetVarNameExpr (Identifier "foo")
                                      (Symbol "[")
                                      (Expr (IntegerConstant 1) [])
                                      (Symbol "]"))
                                    (Symbol "=")
                                    (Expr (IntegerConstant 5) [])
                                    (Symbol ";")))
                         $ parseIt letP "let foo[1] = 5;")

tIfStatementNoElse = TestCase (assertEqual "if statement no else"
                                (Right
                                  (If (Keyword "if")
                                    (Symbol "(")
                                    (Expr (IntegerConstant 1)
                                      [(Op (Symbol "+"), IntegerConstant 2),
                                       (Op (Symbol "="), IntegerConstant 3)])
                                    (Symbol ")")
                                    (Symbol "{")
                                    [Let (Keyword "let")
                                      (LetVarName (Identifier "foo"))
                                      (Symbol "=")
                                      (Expr (IntegerConstant 6) [])
                                      (Symbol ";")]
                                    (Symbol "}")
                                    Nothing))
                                $ parseIt ifP "if (1 + 2 = 3) {let foo = 6;}")

tIfStatementElse = TestCase (assertEqual "if statement with else"
                              (Right
                                (If (Keyword "if")
                                  (Symbol "(")
                                  (Expr (IntegerConstant 1)
                                    [(Op (Symbol "+"), IntegerConstant 2),
                                     (Op (Symbol "="), IntegerConstant 3)])
                                  (Symbol ")")
                                  (Symbol "{")
                                  [Let (Keyword "let")
                                    (LetVarName (Identifier "foo"))
                                    (Symbol "=")
                                    (Expr (IntegerConstant 6) [])
                                    (Symbol ";")]
                                  (Symbol "}")
                                  (Just
                                    (Else (Keyword "else")
                                      (Symbol "{")
                                      [Let (Keyword "let")
                                        (LetVarName (Identifier "foo"))
                                        (Symbol "=")
                                        (Expr (IntegerConstant 2) [])
                                        (Symbol ";")]
                                      (Symbol "}")))))
                              $ parseIt ifP
                              "if ( 1 + 2 = 3) {let foo = 6;} else {let foo = 2;}")

tWhileStatement = TestCase (assertEqual "while block"
                             (Right
                               (While (Keyword "while")
                                 (Symbol "(")
                                 (Expr (VarName (Identifier "foo"))
                                   [(Op (Symbol "="), KeywordConstant "true")])
                                 (Symbol ")")
                                 (Symbol "{")
                                 [Let (Keyword "let")
                                   (LetVarName (Identifier "bar"))
                                   (Symbol "=")
                                   (Expr (IntegerConstant 7) [])
                                   (Symbol ";")]
                                 (Symbol "}")))
                             $ parseIt whileP "while (foo = true) {let bar = 7;}")

tDoSubCallStatement = TestCase (assertEqual "do what.huh()"
                                 (Right
                                   (Do
                                     (Keyword "do")
                                     (SubCallClassOrVar (Identifier "what")
                                      (Symbol ".")
                                      (Identifier "huh")
                                      (Symbol "(")
                                      []
                                      (Symbol ")"))
                                     (Symbol ";")))
                                 $ parseIt doP "do what.huh() ;")

tReturnStatementWithExpr = TestCase (assertEqual "return 1 + 2;"
                                      (Right
                                        (Return
                                          (Keyword "return")
                                          (Just (Expr (IntegerConstant 1)
                                                  [(Op (Symbol "+"), IntegerConstant 2)]))
                                          (Symbol ";")))
                                      $ parseIt returnP "return 1 +     2;")

tReturnNoExpr = TestCase (assertEqual "return;"
                           (Right
                             (Return
                               (Keyword "return")
                               Nothing
                               (Symbol ";")))
                         $ parseIt returnP "return;")

-- Program structure tests
tVarDecOneVar = TestCase (assertEqual "var int foo;"
                           (Right
                             (VarDec (Keyword "var")
                               (TKeyword (Keyword "int"))
                               (Identifier "foo")
                               []
                               (Symbol ";")))
                           $ parseIt varDecP "var int foo;")

tVarDecManyVars = TestCase (assertEqual "var boolean foo, bar, baz;"
                             (Right
                               (VarDec (Keyword "var")
                               (TKeyword (Keyword "boolean"))
                               (Identifier "foo")
                               [(Identifier "bar"),
                                (Identifier "baz")]
                               (Symbol ";")))
                           $ parseIt varDecP "var boolean foo   , bar, baz;")

tSubroutineBody = TestCase (assertEqual "a simple subroutine body"
                             (Right
                               (SubroutineBody (Symbol "{")
                                 [VarDec (Keyword "var")
                                   (TKeyword (Keyword "int"))
                                   (Identifier "foo")
                                   []
                                   (Symbol ";"),
                                   VarDec (Keyword "var")
                                   (TKeyword (Keyword "boolean"))
                                   (Identifier "bar")
                                   [] (Symbol ";")]
                                 [Let (Keyword "let")
                                   (LetVarName (Identifier "foo"))
                                   (Symbol "=")
                                   (Expr (IntegerConstant 5) [])
                                   (Symbol ";"),
                                  Let (Keyword "let")
                                   (LetVarName (Identifier "bar"))
                                   (Symbol "=")
                                   (Expr (KeywordConstant "true") [])
                                   (Symbol ";")]
                                 (Symbol "}")))
                             $ parseIt subroutineBodyP
                             "{var int foo; var boolean bar; let foo = 5; let bar = true;}")

tSubroutineBodyNoVars = TestCase (assertEqual "subroutine body with no vars"
                                  (Right
                                    (SubroutineBody (Symbol "{")
                                      []
                                      [(Do
                                         (Keyword "do")
                                         (SubCallClassOrVar (Identifier "what")
                                           (Symbol ".")
                                           (Identifier "huh")
                                           (Symbol "(")
                                           []
                                           (Symbol ")"))
                                       (Symbol ";"))]
                                     (Symbol "}")))
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
                                (Symbol "(")
                                (ParameterList
                                  [(TKeyword (Keyword "int"),
                                    Identifier "arg1"),
                                    (TKeyword (Keyword "boolean"),
                                     Identifier "arg2")])
                                (Symbol ")")
                                (SubroutineBody (Symbol "{")
                                  [VarDec (Keyword "var")
                                    (TKeyword (Keyword "int"))
                                    (Identifier "baz")
                                    []
                                    (Symbol ";")]
                                  [Let (Keyword "let")
                                    (LetVarName (Identifier "baz"))
                                    (Symbol "=")
                                    (Expr (VarName (Identifier "arg1"))
                                      [(Op (Symbol "+"), IntegerConstant 5)])
                                    (Symbol ";"),
                                    Return (Keyword "return")
                                    Nothing (Symbol ";")]
                                  (Symbol "}"))))
                          $ parseIt subroutineDecP
                          "method void foo (int arg1, boolean arg2) \
                          \ { var int baz; let baz = arg1 + 5; return;}")


tClassVarDecManyVars = TestCase (assertEqual "var boolean foo, bar, baz;"
                                  (Right
                                    (ClassVarDec (Keyword "static")
                                      (TKeyword (Keyword "int"))
                                      (Identifier "foo")
                                      [(Identifier "bar")]
                                     (Symbol ";")))
                           $ parseIt classVarDecP "static int foo, bar;")

tClassWithBasicElems = TestCase (assertEqual "a basic class"
                                 (Right
                                  (Class (Keyword "class")
                                    (Identifier "foo")
                                    (Symbol "{")
                                    [ClassVarDec
                                      (Keyword "static")
                                      (TKeyword (Keyword "int"))
                                      (Identifier "foo")
                                      [Identifier "bar"]
                                      (Symbol ";"),
                                     ClassVarDec
                                      (Keyword "static")
                                      (TKeyword (Keyword "boolean"))
                                      (Identifier "baz")
                                      [Identifier "buz"]
                                      (Symbol ";")]
                                    [SubroutineDec (Keyword "method")
                                      (TKeyword (Keyword "void"))
                                      (Identifier "doFoo")
                                      (Symbol "(")
                                      (ParameterList
                                        [(TKeyword (Keyword "int"),
                                          Identifier "foo"),
                                          (TKeyword (Keyword "boolean"),
                                           Identifier "baz")])
                                      (Symbol ")")
                                      (SubroutineBody
                                        (Symbol "{")
                                        []
                                        [Return (Keyword "return")
                                          (Just
                                            (Expr
                                              (VarName (Identifier "foo")) []))
                                          (Symbol ";")]
                                        (Symbol "}"))]
                                    (Symbol "}")))
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
