import Test.HUnit
import qualified Text.Parsec as Ps
import Data.Either

import AST
import Parser
import XMLGen
import ParserTests

-- helper to join seperated XML tags like "foo \  \ bar \  \ baz"
joinTags :: String -> String
joinTags = concat . words

tKeywordX = TestCase (assertEqual "Keyword -> XML"
                      "<keyword>class</keyword>"
                      $ xKeyword
                      $ fromRight (Keyword "fail")
                      $ parseIt (keywordP "class") "class")

tSymbolX = TestCase (assertEqual "Symbol -> XML"
                     "<symbol>(</symbol>"
                     $ xSymbol
                     $ fromRight (Symbol "fail")
                     $ parseIt (symP "(") "(")

tIdentifierX = TestCase (assertEqual "Identifier -> XML"
                         "<identifier>foo</identifier>"
                         $ xIdentifier
                         $ fromRight (Identifier "fail")
                         $ parseIt identifierP "foo")

tTermIntConstX = TestCase (assertEqual "IntegerConstant -> XML"
                           "<term><integerConstant>5</integerConstant></term>"
                           $ xTerm
                           $ fromRight (IntegerConstant 0)
                           $ parseIt termP "5")

tTermStrConstX = TestCase (assertEqual "StringConstant -> XML"
                           "<term><stringConstant>foo</stringConstant></term>"
                           $ xTerm
                           $ fromRight (StringConstant "fail")
                           $ parseIt termP "\"foo\"")

tExprX = TestCase (assertEqual "Expr -> XML"
                   (joinTags
                    "<expression> \
                      \ <term> \
                        \ <integerConstant>5</integerConstant> \
                      \ </term> \
                      \ <symbol>+</symbol> \
                      \ <term> \
                        \ <integerConstant>10</integerConstant> \
                      \ </term> \
                    \ </expression>")
                   $ xExpr
                   $ fromRight (Expr (StringConstant "fail") [])
                   $ parseIt exprP "5 + 10")

tSubCallNameX = TestCase (assertEqual "SubCallName -> XML"
                          (joinTags
                           "<term> \
                             \ <identifier>foo</identifier> \
                             \ <symbol>(</symbol> \
                             \ <symbol>)</symbol> \
                           \ </term>")
                          $ xTerm
                          $ fromRight (SubroutineCall
                                        (SubCallName (Identifier "fail")
                                          (Symbol "x")
                                          []
                                          (Symbol "x")))
                          $ parseIt termP "foo()")

tSubClassOrVarX = TestCase (assertEqual "SubCallClassOrVar -> XML"
                           (joinTags
                            "<term>< \
                              \ identifier>what</identifier> \
                              \ <symbol>.</symbol> \
                              \ <identifier>huh</identifier> \
                              \ <symbol>(</symbol> \
                              \ <symbol>)</symbol> \
                            \ </term>")
                           $ xTerm
                           $ fromRight (SubroutineCall
                                         (SubCallClassOrVar (Identifier "fail")
                                           (Symbol "x")
                                           (Identifier "fail")
                                           (Symbol "x")
                                           []
                                           (Symbol "x")))
                           $ parseIt termP "what.huh()")


terminalElementTests =
  TestList [TestLabel "Keyword -> XML" tKeywordX,
            TestLabel "Symbol -> XML" tSymbolX,
            TestLabel "IntegerConstant -> XML" tTermIntConstX,
            TestLabel "StringConstant -> XML" tTermStrConstX,
            TestLabel "Identifier -> XML" tIdentifierX]

nonTerminalTests =
  TestList [TestLabel "Expr -> XML" tExprX,
            TestLabel "SubCallName -> XML" tSubCallNameX,
            TestLabel "SubCallClassOrVar -> XML" tSubClassOrVarX]

runXMLTests :: Test
runXMLTests =
  TestList
  $ concat $ [l | (TestList l) <- [terminalElementTests,
                                  nonTerminalTests]]
