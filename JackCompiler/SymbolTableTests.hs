import Test.HUnit
import qualified Data.Map as M

import SymbolTable
import AST
import Parser
import XMLTests

tClassSymbolTable = TestCase (assertEqual "build a symbol table from a class"
                             (Just
                              $ M.fromList
                               [(Identifier "foo",
                                  SymbolInfo {typ = TKeyword (Keyword "int"),
                                              kind = Keyword "static",
                                              occurrence = 0}),
                                (Identifier "bar",
                                  SymbolInfo {typ = TKeyword (Keyword "int"),
                                              kind = Keyword "static",
                                              occurrence = 1}),
                                 (Identifier "baz",
                                   SymbolInfo {typ = TKeyword (Keyword "boolean"),
                                               kind = Keyword "field",
                                               occurrence = 0}),
                                 (Identifier "buz",
                                   SymbolInfo {typ = TKeyword (Keyword "boolean"),
                                               kind = Keyword "field",
                                               occurrence = 1}),
                                 (Identifier "blarg",
                                   SymbolInfo {typ = TKeyword (Keyword "int"),
                                               kind = Keyword "static",
                                               occurrence = 2}),
                                 (Identifier "haha",
                                   SymbolInfo {typ = TKeyword (Keyword "int"),
                                               kind = Keyword "static",
                                               occurrence = 3})])
                             $ fmap classVarSymTable
                             $ tryParse classP
                             "class foo { static int foo, bar; \
                                        \ field boolean baz, buz; \
                                        \ static int blarg, haha; \
                                        \ method void doFoo (int foo, boolean baz) \
                                        \ { return foo; }}")

symTableTests =
  TestList [TestLabel "sym table from class" tClassSymbolTable]

runSymTableTests :: Test
runSymTableTests =
  TestList
  $ concat [l | (TestList l) <- [symTableTests]]

