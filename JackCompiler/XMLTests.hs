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

-- test helper to avoid needing to supply a default value of a matching type
-- to 'fromRight' which becomes awkward as input size grows.
tryParse :: Ps.Parsec String () a -> String -> (Maybe a)
tryParse p s = case parseIt p s of
                 (Right result) -> (Just result)
                 (Left _) -> Nothing

tKeywordX = TestCase (assertEqual "Keyword -> XML"
                      (Just "<keyword>class</keyword>")
                      $ fmap xKeyword
                      $ tryParse (keywordP "class") "class")

tSymbolX = TestCase (assertEqual "Symbol -> XML"
                     (Just "<symbol>(</symbol>")
                     $ fmap xSymbol
                     $ tryParse (symP "(") "(")

tIdentifierX = TestCase (assertEqual "Identifier -> XML"
                         (Just "<identifier>foo</identifier>")
                         $ fmap xIdentifier
                         $ tryParse identifierP "foo")

tTermIntConstX = TestCase (assertEqual "IntegerConstant -> XML"
                           (Just (joinTags
                                  "<term> \
                                  \ <integerConstant>5</integerConstant> \
                                 \ </term>"))
                           $ fmap xTerm
                           $ tryParse termP "5")

tTermStrConstX = TestCase (assertEqual "StringConstant -> XML"
                           (Just (joinTags
                                  "<term> \
                                    \ <stringConstant>foo</stringConstant> \
                                  \ </term>"))
                           $ fmap xTerm
                           $ tryParse termP "\"foo\"")

tTermKeywordConstX = TestCase (assertEqual "KeywordConstant -> XML"
                              (Just (joinTags
                                    "<term> \
                                      \ <keywordConstant>null</keywordConstant> \
                                    \</term>"))
                                $ fmap xTerm
                                $ tryParse termP "null")

tTermVarNameX = TestCase (assertEqual "VarName -> XML"
                         (Just
                          "<term><identifier>foo</identifier></term>")
                         $ fmap xTerm
                         $ tryParse termP "foo")

tExprX = TestCase (assertEqual "Expr -> XML"
                  (Just
                   (joinTags
                    "<expression> \
                      \ <term> \
                        \ <integerConstant>5</integerConstant> \
                      \ </term> \
                      \ <symbol>+</symbol> \
                      \ <term> \
                        \ <integerConstant>10</integerConstant> \
                      \ </term> \
                    \ </expression>"))
                   $ fmap xExpr
                   $ tryParse exprP "5 + 10")

tSubCallNameX = TestCase (assertEqual "SubCallName -> XML"
                         (Just
                          (joinTags
                           "<term> \
                             \ <identifier>foo</identifier> \
                             \ <symbol>(</symbol> \
                             \ <symbol>)</symbol> \
                           \ </term>"))
                          $ fmap xTerm
                          $ tryParse termP "foo()")

tSubClassOrVarX = TestCase (assertEqual "SubCallClassOrVar -> XML"
                           (Just
                            (joinTags
                             "<term>< \
                                \ identifier>what</identifier> \
                                \ <symbol>.</symbol> \
                                \ <identifier>huh</identifier> \
                                \ <symbol>(</symbol> \
                                \ <symbol>)</symbol> \
                              \ </term>"))
                           $ fmap xTerm
                           $ tryParse termP "what.huh()")

tVarNameArrayAccessX = TestCase (assertEqual "VarNameExpr -> XML"
                                (Just
                                 (joinTags
                                  "<term> \
                                    \ <identifier>foo</identifier> \
                                    \ <symbol>[</symbol> \
                                    \ <expression> \
                                      \ <term> \
                                      \ <integerConstant>1</integerConstant> \
                                      \ </term> \
                                    \ </expression> \
                                    \ <symbol>]</symbol> \
                                  \ </term>"))
                                $ fmap xTerm
                                $ tryParse termP "foo[1]")

tTermUnaryOpX = TestCase (assertEqual "UnaryOp -> XML"
                         -- TODO: check that the nested term tags is what's expected
                         -- when xTerm gets recursively called.
                         -- this could cause the coursera tests to fail
                         (Just
                           (joinTags
                            "<term> \
                              \ <symbol>~</symbol> \
                            \ <term> \
                              \ <identifier>foo</identifier> \
                            \ </term> \
                          \ </term>"))
                         $ fmap xTerm
                         $ tryParse termP "~foo")

tTermParenExprX = TestCase (assertEqual "ParenExpr -> XML"
                           (Just
                             (joinTags
                              "<term> \
                              \ <symbol>(</symbol> \
                              \ <expression> \
                                \ <term> \
                                  \ <integerConstant>1</integerConstant> \
                                \ </term> \
                                \ <symbol>+</symbol> \
                                \ <term> \
                                  \ <integerConstant>2</integerConstant> \
                                \ </term> \
                              \ </expression> \
                              \ <symbol>)</symbol> \
                            \ </term>"
                             ))
                           $ fmap xTerm
                           $ tryParse termP "(1 + 2)")

terminalElementTests =
  TestList [TestLabel "Keyword -> XML" tKeywordX,
            TestLabel "Symbol -> XML" tSymbolX,
            TestLabel "IntegerConstant -> XML" tTermIntConstX,
            TestLabel "StringConstant -> XML" tTermStrConstX,
            TestLabel "Identifier -> XML" tIdentifierX,
            TestLabel "KeywordConstant -> XML" tTermKeywordConstX,
            TestLabel "VarName -> XML" tTermVarNameX]

nonTerminalTests =
  TestList [TestLabel "Expr -> XML" tExprX,
            TestLabel "SubCallName -> XML" tSubCallNameX,
            TestLabel "SubCallClassOrVar -> XML" tSubClassOrVarX,
            TestLabel "VarNameExpr -> XML" tVarNameArrayAccessX,
            TestLabel "UnaryOp -> XML" tTermUnaryOpX,
            TestLabel "ParenExpr -> XML" tTermParenExprX]

runXMLTests :: Test
runXMLTests =
  TestList
  $ concat $ [l | (TestList l) <- [terminalElementTests,
                                  nonTerminalTests]]
