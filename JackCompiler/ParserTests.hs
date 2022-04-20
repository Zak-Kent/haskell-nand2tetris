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
                       $ parseIt keywordP "   class   ")

tKeywordPFail = TestCase (assertBool "keywordP fail"
                           (isLeft $ parseIt keywordP "x"))

tSymbolP = TestCase (assertEqual "symbol parse"
                      (Right (Symbol "+"))
                      $ parseIt symbolP "+")

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
                                [(Op "+", (IntegerConstant 8))]))
                        $ parseIt exprP "5 + 8")

tExprManyTerms = TestCase (assertEqual "an expression with many terms parses"
                        (Right (Expr (VarName (Identifier "foo"))
                                [(Op "=", (IntegerConstant 4)),
                                 (Op "+", (IntegerConstant 2))]))
                        $ parseIt exprP "foo = 4 + 2")

-- subCallNameP tests
tBasicSubCall = TestCase (assertEqual "a sub routine with expr list parses"
                         (Right (SubCallName (Identifier "foo")
                                (Symbol "(")
                                [(Expr (IntegerConstant 5)
                                  [(Op "+", (IntegerConstant 8))]),
                                  (Expr (IntegerConstant 10)
                                  [(Op "-", (IntegerConstant 42))])]
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
                                           [(Op "=", (IntegerConstant 5))]),
                                          (Expr (IntegerConstant 10)
                                           [(Op "-", (IntegerConstant 42))])]
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
                                     [(Op "+", (IntegerConstant 2))])]
                                   (Symbol ")"))))
                                 $ parseIt subroutineCallP "things(1+2)")

-- varNameExprP tests
tVarNameArrayAcccess = TestCase (assertEqual "array access using []"
                                  (Right
                                    (VarNameExpr (Identifier "foo")
                                      (Symbol "[")
                                      (Expr (IntegerConstant 1)
                                        [(Op "+", (IntegerConstant 2))])
                                      (Symbol "]")))
                                  $ parseIt varNameExprP "foo[1 + 2]")

-- parenExprP tests
tParensWrappingExpression = TestCase (assertEqual "parens wrapping expression"
                                       (Right
                                         (ParenExpr
                                           (Symbol "(")
                                           (Expr (IntegerConstant 1)
                                            [(Op "+", (IntegerConstant 2)),
                                             (Op "+", (IntegerConstant 3)),
                                             (Op "+", (IntegerConstant 4))])
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
                             (LetS
                               (Let (Symbol "let")
                                 (LetVarName (Identifier "foo"))
                                 (Symbol "=")
                                 (Expr (IntegerConstant 5) [])
                                 (Symbol ";"))))
                         $ parseIt letP "let foo = 5;")

tLetStatementWExpr = TestCase (assertEqual "let statement with expr varname"
                                (Right
                                  (LetS
                                    (Let (Symbol "let")
                                      (LetVarNameExpr (Identifier "foo")
                                        (Symbol "[")
                                        (Expr (IntegerConstant 1) [])
                                        (Symbol "]"))
                                      (Symbol "=")
                                      (Expr (IntegerConstant 5) [])
                                      (Symbol ";"))))
                         $ parseIt letP "let foo[1] = 5;")

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
letStatmentTests =
  TestList [TestLabel "let statment: let foo = 5;" tLetStatement,
            TestLabel "let statment: let foo[1] = 5;" tLetStatementWExpr]

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
                                   letStatmentTests]]

-- run in REPL with: runTestTT (<TestList> | runAllTests)
