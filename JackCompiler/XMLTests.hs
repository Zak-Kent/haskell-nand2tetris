module XMLTests where

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
                      $ fmap joinTags
                      $ fmap genXML
                      $ tryParse (keywordP "class") "class")

tSymbolX = TestCase (assertEqual "Symbol -> XML"
                     (Just "<symbol>(</symbol>")
                     $ fmap joinTags
                     $ fmap genXML
                     $ tryParse (symP "(") "(")

tIdentifierX = TestCase (assertEqual "Identifier -> XML"
                         (Just "<identifier>foo</identifier>")
                         $ fmap joinTags
                         $ fmap genXML
                         $ tryParse identifierP "foo")

tTermIntConstX = TestCase (assertEqual "IntegerConstant -> XML"
                           (Just (joinTags
                                  "<term> \
                                  \ <integerConstant>5</integerConstant> \
                                 \ </term>"))
                           $ fmap joinTags
                           $ fmap genXML
                           $ tryParse termP "5")

tTermStrConstX = TestCase (assertEqual "StringConstant -> XML"
                           (Just (joinTags
                                  "<term> \
                                    \ <stringConstant>foo</stringConstant> \
                                  \ </term>"))
                           $ fmap joinTags
                           $ fmap genXML
                           $ tryParse termP "\"foo\"")

tTermKeywordConstX = TestCase (assertEqual "KeywordConstant -> XML"
                              (Just (joinTags
                                    "<term> \
                                      \ <keyword>null</keyword> \
                                    \</term>"))
                                $ fmap joinTags
                                $ fmap genXML
                                $ tryParse termP "null")

tTermVarNameX = TestCase (assertEqual "VarName -> XML"
                         (Just
                          "<term><identifier>foo</identifier></term>")
                         $ fmap joinTags
                         $ fmap genXML
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
                   $ fmap joinTags
                   $ fmap genXML
                   $ tryParse exprP "5 + 10")

tSubCallNameX = TestCase (assertEqual "SubCallName -> XML"
                         (Just
                          (joinTags
                            "<term> \
                             \ <identifier>foo</identifier> \
                             \ <symbol>(</symbol> \
                             \ <expressionList></expressionList> \
                             \ <symbol>)</symbol> \
                           \ </term>"))
                          $ fmap joinTags
                          $ fmap genXML
                          $ tryParse termP "foo()")

tSubClassOrVarX = TestCase (assertEqual "SubCallClassOrVar -> XML"
                           (Just
                            (joinTags
                              "<term>< \
                                \ identifier>what</identifier> \
                                \ <symbol>.</symbol> \
                                \ <identifier>huh</identifier> \
                                \ <symbol>(</symbol> \
                                \ <expressionList></expressionList> \
                                \ <symbol>)</symbol> \
                              \ </term>"))
                           $ fmap joinTags
                           $ fmap genXML
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
                                $ fmap joinTags
                                $ fmap genXML
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
                         $ fmap joinTags
                         $ fmap genXML
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
                            \ </term>"))
                           $ fmap joinTags
                           $ fmap genXML
                           $ tryParse termP "(1 + 2)")

tStatementLetX = TestCase (assertEqual "LetVarName -> XML"
                          (Just
                            (joinTags
                              "<letStatement> \
                                \ <keyword>let</keyword> \
                                \ <identifier>foo</identifier> \
                                \ <symbol>=</symbol> \
                                \ <expression> \
                                  \ <term> \
                                    \ <integerConstant>5</integerConstant> \
                                  \ </term> \
                                \ </expression> \
                                \ <symbol>;</symbol> \
                              \ </letStatement>"))
                          $ fmap joinTags
                          $ fmap genXML
                          $ tryParse statementP "let foo = 5;")

tStatementLetVNExprX = TestCase (assertEqual "LetVarNameExpr -> XML"
                                (Just
                                  (joinTags
                                   "<letStatement> \
                                    \ <keyword>let</keyword> \
                                    \ <identifier>foo</identifier> \
                                    \ <symbol>[</symbol> \
                                    \ <expression> \
                                      \ <term> \
                                        \ <integerConstant>5</integerConstant> \
                                      \ </term> \
                                    \ </expression> \
                                    \ <symbol>]</symbol> \
                                    \ <symbol>=</symbol> \
                                    \ <expression> \
                                      \ <term> \
                                        \ <integerConstant>12</integerConstant> \
                                      \ </term> \
                                    \ </expression> \
                                    \ <symbol>;</symbol> \
                                  \ </letStatement>"))
                                $ fmap joinTags
                                $ fmap genXML
                                $ tryParse statementP "let foo[5] = 12;")

tAllStatementsX = TestCase (assertEqual "AllStatements -> XML"
                                (Just
                                  (joinTags
                                    "<ifStatement> \
                                      \ <keyword>if</keyword> \
                                      \ <symbol>(</symbol> \
                                      \ <expression> \
                                        \ <term> \
                                          \ <keyword>true</keyword> \
                                        \ </term> \
                                      \ </expression> \
                                      \ <symbol>)</symbol> \
                                      \ <symbol>{</symbol> \
                                      \ <statements> \
                                      \ <letStatement> \
                                        \ <keyword>let</keyword> \
                                        \ <identifier>foo</identifier> \
                                        \ <symbol>=</symbol> \
                                        \ <expression> \
                                          \ <term> \
                                            \ <integerConstant>5</integerConstant> \
                                          \ </term> \
                                        \ </expression> \
                                        \ <symbol>;</symbol> \
                                      \ </letStatement> \
                                      \ <doStatement> \
                                        \ <keyword>do</keyword> \
                                        \ <identifier>bar</identifier> \
                                        \ <symbol>(</symbol> \
                                         \ <expressionList> \
                                          \ <expression> \
                                            \ <term> \
                                              \ <integerConstant>1</integerConstant> \
                                            \ </term> \
                                          \ </expression> \
                                          \ <symbol>,</symbol> \
                                          \ <expression> \
                                            \ <term> \
                                              \ <integerConstant>2</integerConstant> \
                                            \ </term> \
                                          \ </expression> \
                                        \ </expressionList> \
                                        \ <symbol>)</symbol> \
                                        \ <symbol>;</symbol> \
                                      \ </doStatement> \
                                      \ <returnStatement> \
                                        \ <keyword>return</keyword> \
                                        \ <expression> \
                                          \ <term> \
                                            \ <integerConstant>42</integerConstant> \
                                          \ </term> \
                                        \ </expression> \
                                        \ <symbol>;</symbol> \
                                      \ </returnStatement> \
                                      \ </statements> \
                                      \ <symbol>}</symbol> \
                                      \ <keyword>else</keyword> \
                                      \ <symbol>{</symbol> \
                                      \ <statements> \
                                      \ <whileStatement> \
                                        \ <keyword>while</keyword> \
                                        \ <symbol>(</symbol> \
                                        \ <expression> \
                                          \ <term> \
                                            \ <keyword>false</keyword> \
                                          \ </term> \
                                        \ </expression> \
                                        \ <symbol>)</symbol> \
                                        \ <symbol>{</symbol> \
                                        \ <statements> \
                                        \ <doStatement> \
                                          \ <keyword>do</keyword> \
                                          \ <identifier>bar</identifier> \
                                          \ <symbol>.</symbol> \
                                          \ <identifier>baz</identifier> \
                                          \ <symbol>(</symbol> \
                                          \ <expressionList></expressionList> \
                                          \ <symbol>)</symbol> \
                                          \ <symbol>;</symbol> \
                                        \ </doStatement> \
                                        \ </statements> \
                                        \ <symbol>}</symbol> \
                                      \ </whileStatement> \
                                      \ </statements> \
                                      \ <symbol>}</symbol> \
                                    \ </ifStatement>"))
                                $ fmap joinTags
                                $ fmap genXML
                                $ tryParse statementP "if (true) \
                                                      \ {let foo = 5; \
                                                      \  do bar(1, 2);  \
                                                      \  return 42; } \
                                                      \ else {while (false) \
                                                      \      {do bar.baz();}};")

tVarDecX = TestCase (assertEqual "VarDec -> XML"
                    (Just
                      (joinTags
                         "<varDec> \
                            \ <keyword>var</keyword> \
                            \ <keyword>int</keyword> \
                            \ <identifier>foo</identifier> \
                            \ <symbol>,</symbol> \
                            \ <identifier>bar</identifier> \
                            \ <symbol>;</symbol> \
                          \ </varDec>"))
                    $ fmap joinTags
                    $ fmap genXML
                    $ tryParse varDecP "var int foo, bar;")

tSubroutineBodyX = TestCase (assertEqual "SubroutingBody -> XML"
                            (Just (joinTags
                                    "<subroutineBody> \
                                      \ <symbol>{</symbol> \
                                      \ <varDec> \
                                        \ <keyword>var</keyword> \
                                        \ <keyword>int</keyword> \
                                        \ <identifier>foo</identifier> \
                                        \ <symbol>;</symbol> \
                                      \ </varDec> \
                                      \ <statements> \
                                      \ <letStatement> \
                                        \ <keyword>let</keyword> \
                                        \ <identifier>foo</identifier> \
                                        \ <symbol>=</symbol> \
                                        \ <expression> \
                                          \ <term> \
                                            \ <integerConstant>5</integerConstant> \
                                          \ </term> \
                                        \ </expression> \
                                        \ <symbol>;</symbol> \
                                      \ </letStatement> \
                                      \ </statements> \
                                      \ <symbol>}</symbol> \
                                      \ </subroutineBody>"))
                            $ fmap joinTags
                            $ fmap genXML
                            $ tryParse subroutineBodyP "{var int foo; \
                                                       \ let foo = 5;}")

tParamListX = TestCase (assertEqual "ParamterList -> XML"
                       (Just
                        (joinTags
                          " <parameterList> \
                            \ <keyword>int</keyword> \
                            \ <identifier>foo</identifier> \
                            \ <symbol>,</symbol> \
                            \ <keyword>boolean</keyword> \
                            \ <identifier>bar</identifier> \
                          \ </parameterList>"))
                         $ fmap joinTags
                         $ fmap genXML
                         $ tryParse paramListP "int foo, boolean bar")

tSubroutineDecX = TestCase (assertEqual "SubroutineDec -> XML"
                           (Just
                             (joinTags
                               "<subroutineDec> \
                                 \ <keyword>method</keyword> \
                                 \ <keyword>void</keyword> \
                                 \ <identifier>foo</identifier> \
                                 \ <symbol>(</symbol> \
                                 \ <parameterList></parameterList> \
                                 \ <symbol>)</symbol> \
                                 \ <subroutineBody> \
                                   \ <symbol>{</symbol> \
                                   \ <statements> \
                                   \ <doStatement> \
                                     \ <keyword>do</keyword> \
                                     \ <identifier>it</identifier> \
                                     \ <symbol>(</symbol> \
                                     \ <expressionList></expressionList> \
                                     \ <symbol>)</symbol> \
                                     \<symbol>;</symbol> \
                                   \ </doStatement> \
                                   \ </statements> \
                                   \ <symbol>}</symbol> \
                                 \ </subroutineBody> \
                               \ </subroutineDec>"))
                           $ fmap joinTags
                           $ fmap genXML
                           $ tryParse subroutineDecP "method void foo () {do it();}")

tClassVarDecX = TestCase (assertEqual "ClassVarDec -> XML"
                         (Just
                           (joinTags
                             "<classVarDec> \
                               \ <keyword>static</keyword> \
                               \ <keyword>int</keyword> \
                               \ <identifier>foo</identifier> \
                               \ <symbol>,</symbol> \
                               \ <identifier>bar</identifier> \
                               \ <symbol>;</symbol> \
                             \ </classVarDec>"))
                         $ fmap joinTags
                         $ fmap genXML
                         $ tryParse classVarDecP "static int foo, bar;")

tClassX = TestCase (assertEqual "Class -> XML"
                   (Just
                     (joinTags
                       "<class> \
                         \ <keyword>class</keyword> \
                         \ <identifier>foo</identifier> \
                         \ <symbol>{</symbol> \
                         \ <classVarDec>\n \
                           \ <keyword>static</keyword> \
                           \ <keyword>int</keyword> \
                           \ <identifier>foo</identifier> \
                           \ <symbol>;</symbol> \
                         \ </classVarDec> \
                         \ <classVarDec> \
                           \ <keyword>field</keyword> \
                           \ <keyword>boolean</keyword> \
                           \ <identifier>bar</identifier> \
                           \ <symbol>;</symbol> \
                         \ </classVarDec> \
                         \ <subroutineDec> \
                           \ <keyword>method</keyword> \
                           \ <keyword>void</keyword> \
                           \ <identifier>baz</identifier> \
                           \ <symbol>(</symbol> \
                           \ <parameterList> \
                             \ <keyword>int</keyword> \
                             \ <identifier>biz</identifier> \
                           \ </parameterList> \
                           \ <symbol>)</symbol> \
                           \ <subroutineBody> \
                             \ <symbol>{</symbol>\n \
                             \ <statements> \
                             \ <doStatement> \
                               \ <keyword>do</keyword> \
                               \ <identifier>blarg</identifier> \
                               \ <symbol>(</symbol> \
                              \ <expressionList> \
                               \ <expression> \
                                 \ <term> \
                                   \ <identifier>biz</identifier> \
                                 \ </term> \
                               \ </expression> \
                              \ </expressionList> \
                               \ <symbol>)</symbol>\n \
                               \ <symbol>;</symbol>\n \
                             \ </doStatement> \
                             \ </statements> \
                             \ <symbol>}</symbol> \
                           \ </subroutineBody>\n \
                         \ </subroutineDec> \
                         \ <symbol>}</symbol> \
                       \ </class>"))
                   $ fmap joinTags
                   $ fmap genXML
                   $ tryParse classP "// comment 1 \n //comment 2 \n \
                                     \ class foo {static int foo; \
                                              \/** block comment */ \
                                               \ field boolean bar; \
                                               \ method void baz (int biz) \
                                               \ { do blarg(biz); }}")

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
            TestLabel "ParenExpr -> XML" tTermParenExprX,
            TestLabel "LetVarName -> XML" tStatementLetX,
            TestLabel "LetVarNameExpr -> XML" tStatementLetVNExprX,
            TestLabel "AllStatements -> XML" tAllStatementsX,
            TestLabel "VarDec -> XML" tVarDecX,
            TestLabel "SubroutineBody -> XML" tSubroutineBodyX,
            TestLabel "ParameterList -> XML" tParamListX,
            TestLabel "SubroutineDec -> XML" tSubroutineDecX,
            TestLabel "ClassVarDec -> XML" tClassVarDecX,
            TestLabel "Class -> XML" tClassX]

runXMLTests :: Test
runXMLTests =
  TestList
  $ concat $ [l | (TestList l) <- [terminalElementTests,
                                  nonTerminalTests]]
