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
                                [((Op "+", (IntegerConstant 8)))]))
                        $ parseIt exprP "5 + 8")

tExprManyTerms = TestCase (assertEqual "an expression with many terms parses"
                        (Right (Expr (VarName (Identifier "foo"))
                                [((Op "=", (IntegerConstant 4))),
                                 ((Op "+", (IntegerConstant 2)))]))
                        $ parseIt exprP "foo = 4 + 2")

-- subCallNameP tests
tBasicSubCall = TestCase (assertEqual "a sub routine with expr list parses"
                         (Right (SubCallName (Identifier "foo")
                                (Symbol "(")
                                [(Expr (IntegerConstant 5)
                                  [((Op "+", (IntegerConstant 8)))]),
                                  (Expr (IntegerConstant 10)
                                  [((Op "-", (IntegerConstant 42)))])]
                                (Symbol ")")))
                           $ parseIt subCallNameP "foo (5+8, 10  - 42)")

tSubCallNoExprs = TestCase (assertEqual "a sub routine with no exprs"
                            (Right (SubCallName (Identifier "bar")
                                    (Symbol "(")
                                    []
                                    (Symbol ")")))
                            $ parseIt subCallNameP "bar()    ")
-- TestLists
terminalParserTests =
  TestList [TestLabel "test keywordP" tKeywordP,
            TestLabel "test keywordP fail" tKeywordPFail,
            TestLabel "test symbolP" tSymbolP]

identifierPTests =
  TestList [TestLabel "test identifierP" tIdentifierP,
            TestLabel "test identifierP spaces" tIdentifierPSpaces,
            TestLabel "test identifierP no digit" tIdentifierPNoDigit]

exprPTests =
  TestList [TestLabel "test exprP one term" tExprOneTerm,
            TestLabel "test exprP two term" tExprTwoTerms,
            TestLabel "test exprP many terms" tExprManyTerms]

subCallNamePTests =
  TestList [TestLabel "test sub routine call with expr list" tBasicSubCall,
            TestLabel "test sub rountne call with no exprs" tSubCallNoExprs]

-- run in REPL with: runTestTT <TestList>
