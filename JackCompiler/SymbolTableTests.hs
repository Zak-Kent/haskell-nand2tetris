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

tSubroutineMethodSymTable = TestCase (assertEqual "subroutine symbol table built correctly"
                                  (Just $ M.fromList [
                                      (Identifier "this",
                                        SymbolInfo {typ = TIdentifier (Identifier "foo"),
                                                    kind = Keyword "argument",
                                                    occurrence = 0}),
                                      (Identifier "arg1",
                                        SymbolInfo {typ = TKeyword (Keyword "int"),
                                                   kind = Keyword "argument",
                                                   occurrence = 1}),
                                      (Identifier "arg2",
                                        SymbolInfo {typ = TKeyword (Keyword "boolean"),
                                                    kind = Keyword "argument",
                                                    occurrence = 2}),
                                      (Identifier "baz",
                                        SymbolInfo {typ = TKeyword (Keyword "int"),
                                                    kind = Keyword "local",
                                                    occurrence = 0}),
                                      (Identifier "blarg",
                                        SymbolInfo {typ = TKeyword (Keyword "int"),
                                                    kind = Keyword "local",
                                                    occurrence = 1}),
                                      (Identifier "booley",
                                        SymbolInfo {typ = TKeyword (Keyword "boolean"),
                                                    kind = Keyword "local",
                                                    occurrence = 2}),
                                      (Identifier "bulp",
                                        SymbolInfo {typ = TKeyword (Keyword "int"),
                                                    kind = Keyword "local",
                                                    occurrence = 3})])
                                  $ fmap (subVarSymTable (TIdentifier (Identifier "foo")))
                                  $ tryParse subroutineDecP
                                  "method void foo (int arg1, boolean arg2) \
                                  \ { var int baz, blarg; var boolean booley; var int bulp; \
                                  \ let baz = arg1 + 5; return;}")

tSubroutineConstructorSymTable = TestCase
  (assertEqual "Subroutine constructor has sym table without 'this' entry"
   (Just $ M.fromList [
       (Identifier "arg1",
         SymbolInfo {typ = TKeyword (Keyword "int"),
                     kind = Keyword "argument",
                     occurrence = 0}),
       (Identifier "arg2",
         SymbolInfo {typ = TKeyword (Keyword "int"),
                     kind = Keyword "argument",
                     occurrence = 1}),
       (Identifier "baz",
         SymbolInfo {typ = TKeyword (Keyword "int"),
                     kind = Keyword "local",
                     occurrence = 0})])
    $ fmap (subVarSymTable (TIdentifier (Identifier "foo")))
    $ tryParse subroutineDecP
    "constructor Point foo (int arg1, int arg2) \
    \ { var int baz; \
    \ let baz = 5; return;}")

symTableTests =
  TestList [TestLabel "class" tClassSymbolTable,
            TestLabel "method subroutine" tSubroutineMethodSymTable,
            TestLabel "constructor subroutine" tSubroutineConstructorSymTable]

runSymTableTests :: Test
runSymTableTests =
  TestList
  $ concat [l | (TestList l) <- [symTableTests]]

