import Test.HUnit
import qualified Control.Monad.State as S
import qualified Data.Map as M

import CodeGen
import AST
import SymbolTable
import Parser
import XMLTests

localSymT = M.fromList [(Identifier "x",
                         SymbolInfo {typ = TKeyword (Keyword "int"),
                                     kind = Keyword "argument",
                                     occurrence = 1}),
                        (Identifier "y",
                         SymbolInfo {typ = TKeyword (Keyword "int"),
                                     kind = Keyword "local",
                                     occurrence = 5})]


classSymT = M.fromList [(Identifier "g",
                         SymbolInfo {typ = TKeyword (Keyword "int"),
                                     kind = Keyword "field",
                                     occurrence = 0}),
                         (Identifier "f1",
                         SymbolInfo {typ = TIdentifier (Identifier "Foo"),
                                     kind = Keyword "field",
                                     occurrence = 1}),
                         (Identifier "z",
                         SymbolInfo {typ = TKeyword (Keyword "int"),
                                     kind = Keyword "static",
                                     occurrence = 2}),
                        (Identifier "y",
                         SymbolInfo {typ = TKeyword (Keyword "int"),
                                     kind = Keyword "local should win",
                                     occurrence = 0})]

evalVM :: (VMGen a) => Maybe a -> Maybe String
evalVM Nothing = Nothing
evalVM (Just vm) =
  Just $ S.evalState (genVM vm) (classSymT, localSymT, 0, 0, "Foo")

checkSymTables ::  (VMGen a) => Maybe a -> Maybe (SymTable, SymTable)
checkSymTables Nothing = Nothing
checkSymTables (Just vm) =
    let (clsSyms, subSyms, _, _, _) =
          S.execState (genVM vm) (M.empty, M.empty, 0, 0, "Foo")
    in Just (clsSyms, subSyms)

evalVMEmptyState :: (VMGen a) => Maybe a -> Maybe String
evalVMEmptyState Nothing = Nothing
evalVMEmptyState (Just vm) =
  Just $ S.evalState (genVM vm) (M.empty, M.empty, 0, 0, "Foo")

tSimpleExpr = TestCase (assertEqual "5 + 6 + 7"
                        (Just
                         (joinTags
                          "push constant 5 \
                          \push constant 6 \
                          \push constant 7 \
                          \add \
                          \add"))
                        $ fmap joinTags
                        $ evalVM
                        $ tryParse exprP "5 + 6 + 7")

tExprWithParens = TestCase (assertEqual "(4 + 2) - 8 + (3 * (2 + 1))"
                            (Just
                             (joinTags
                              "push constant 4 \
                             \ push constant 2 \
                             \ add \
                             \ push constant 8 \
                             \ push constant 3 \
                             \ push constant 2 \
                             \ push constant 1 \
                             \ add \
                             \ call Math.multiply 2 \
                             \ add \
                             \ sub"))
                            $ fmap joinTags
                            $ evalVM
                            $ tryParse exprP "(4 + 2) - 8 + (3 * (2 + 1))")

tExprWithMethodCall = TestCase (assertEqual "x + g(2,y,-z) * 5"
                                (Just
                                 (joinTags
                                  "push argument 1 \
                                 \ push pointer 0 \
                                 \ push constant 2 \
                                 \ push local 5 \
                                 \ push static 2 \
                                 \ neg \
                                 \ call Foo.g 4 \
                                 \ push constant 5 \
                                 \ call Math.multiply 2 \
                                 \ add"))
                                $ fmap joinTags
                                $ evalVM
                                $ tryParse exprP "x + g(2,y,-z) * 5")

tLetStatementCG = TestCase (assertEqual "let x = 5;"
                            (Just
                             (joinTags
                              "push constant 5 \
                             \ pop \
                             \ argument 1"))
                           $ fmap joinTags
                           $ evalVM
                           $ tryParse statementP "let x = 5;")

tLetStatementWExprCG = TestCase (assertEqual "let g = 3 + 5;"
                                 (Just
                                  (joinTags
                                   "push constant 3 \
                                  \ push constant 5 \
                                  \ add \
                                  \ pop this 0"))
                           $ fmap joinTags
                           $ evalVM
                           $ tryParse statementP "let g = 3 + 5;")

tLetStatementWithArrayAccess =
  TestCase (assertEqual "let g = x[1];"
                                 (Just
                                  (joinTags
                                   "push constant 1 \
                                  \ push argument 1 \
                                  \ add \
                                  \ pop pointer 1 \
                                  \ push that 0 \
                                  \ pop this 0"))
                           $ fmap joinTags
                           $ evalVM
                           $ tryParse statementP "let g = x[1];")

tLetStatementVarNameExprCG =
  TestCase (assertEqual "let y[0] = 3;"
            (Just
             (joinTags
              "push constant 0 \
             \ push local 5 \
             \ add \
             \ push constant 3 \
             \ pop temp 0 \
             \ pop pointer 1 \
             \ push temp 0 \
             \ pop that 0"))
             $ fmap joinTags
             $ evalVM
             $ tryParse statementP "let y[0] = 3;")

tDoSubCallStatementCG =
  TestCase (assertEqual "do foo.bar(1, 2);"
            (Just
             (joinTags
              "push constant 1 \
             \ push constant 2 \
             \ call foo.bar 2 \
             \ pop temp 0"))
             $ fmap joinTags
             $ evalVM
             $ tryParse statementP "do foo.bar(1, 2);")

tDoCallThisArg =
  TestCase (assertEqual "do foo.bar(this);"
            (Just
             (joinTags
              "push pointer 0 \
             \ call foo.bar 1 \
             \ pop temp 0"))
             $ fmap joinTags
             $ evalVM
             $ tryParse statementP "do foo.bar(this);")

tDoCallPushesObjPointer =
  TestCase (assertEqual "do f1.foo(1);"
            (Just
             (joinTags
              "push this 1 \
             \ push constant 1 \
             \ call Foo.foo 2 \
             \ pop temp 0"))
             $ fmap joinTags
             $ evalVM
             $ tryParse statementP "do f1.foo(1);")

tReturnNoExprCG =
  TestCase (assertEqual "return"
            (Just (joinTags
                   "push constant 0 \
                  \ return"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "return;")

tReturnWithExprCG =
  TestCase (assertEqual "return 5 * 5;"
            (Just
             (joinTags
              "push constant 5 \
             \ push constant 5 \
             \ call Math.multiply 2 \
             \ return"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "return 5 * 5;")

tIfStatementNoElseCG =
  TestCase (assertEqual "if ((1 + 2) = 3) {let x = 6;}"
           {- Note the parens needed around (1 + 2) this is due to no
              operator precedence in the Jack language and the compiler's
              expression evaluation going from right to left which the
              course spec says is ok -}
            (Just
             (joinTags
              "push constant 1 \
             \ push constant 2 \
             \ add \
             \ push constant 3 \
             \ eq \
             \ if-goto IF_TRUE0 \
             \ goto IF_FALSE0 \
             \ label IF_TRUE0 \
             \ push constant 6 \
             \ pop argument 1 \
             \ label IF_FALSE0"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "if ((1 + 2) = 3) {let x = 6;}")

tIfStatementElseCG =
  TestCase (assertEqual "if (false) {let g = 2;} else {do foo.bar();} "
            (Just
             (joinTags
              "push constant 0 \
             \ if-goto IF_TRUE0 \
             \ goto IF_FALSE0 \
             \ label IF_TRUE0 \
             \ push constant 2 \
             \ pop this 0 \
             \ goto IF_END0 \
             \ label IF_FALSE0 \
             \ call foo.bar 0 \
             \ pop temp 0 \
             \ label IF_END0"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "if (false) {let g = 2;} else {do foo.bar();} ")

tWhileStatementCG =
  TestCase (assertEqual "{while (true) {do bar.baz();}}"
            (Just
             (joinTags
              "label WHILE_EXP0 \
             \ push constant 0 \
             \ not \
             \ not \
             \ if-goto WHILE_END0 \
             \ call bar.baz 0\
             \ pop temp 0 \
             \ goto WHILE_EXP0 \
             \ label WHILE_END0"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "while (true) {do bar.baz();}")

tSingleVarDecSymTableUpdate =
  TestCase (assertEqual "Single VarDec updates subroutine symbol table"
            (Just 2)
            $ fmap (\(clsS, subS) -> length $ M.keys subS)
            $ checkSymTables
            $ tryParse varDecP "var int foo, bar;")

tParamListSymTableUpdate =
  TestCase (assertEqual "ParameterList updates symbol table"
            (Just 3)
            $ fmap (\(clsS, subS) -> length $ M.keys subS)
            $ checkSymTables
            $ tryParse paramListP "int foo, boolean bar, classFoo baz")

tSimpleVoidFunc =
  TestCase (assertEqual "Simple void function"
            (Just
             (joinTags
             "function Foo.bar 2 \
            \ push constant 5 \
            \ pop local 0 \
            \ push constant 0 \
            \ return"))
            $ fmap joinTags
            $ evalVMEmptyState
            $ tryParse subroutineDecP "function void bar () \
                                      \  {var int foo, baz; \
                                      \   let foo = 5; \
                                      \   return;}")

tSimpleIntReturnFunc =
  TestCase (assertEqual "Simple int return function"
            (Just
             (joinTags
              "function Foo.bar 3 \
            \ push constant 10 \
            \ pop local 2 \
            \ push local 2 \
            \ return"))
            $ fmap joinTags
            $ evalVMEmptyState
            $ tryParse subroutineDecP "function int bar () \
                                      \  {var int baz, biz, foo; \
                                      \   let foo = 10; \
                                      \   return foo;}")

tConstructor =
  TestCase (assertEqual "Constructor with one class field"
            (Just
             (joinTags
              "function Foo.new 0 \
             \ push constant 2 \
             \ call Memory.alloc 1 \
             \ pop pointer 0 \
             \ push argument 0 \
             \ pop this 0 \
             \ push pointer 0 \
             \ return"))
            $ fmap joinTags
            $ evalVM
            -- g is field in pre populated class sym table
            $ tryParse subroutineDecP "constructor Foo new(int ax) \
                                      \  { let g = ax; \
                                      \   return this;}")

tMethod =
  TestCase (assertEqual "Method returning an int"
            (Just
             (joinTags
              "function Foo.bar 1 \
             \ push argument 0 \
             \ pop pointer 0 \
             \ push argument 1 \
             \ push constant 5 \
             \ add \
             \ pop local 0 \
             \ push local 0 \
             \ return"))
            $ fmap joinTags
            $ evalVM
            -- g is field in pre populated class sym table
            $ tryParse subroutineDecP "method int bar(int ax) \
                                      \  { var int foo; \
                                      \    let foo = ax + 5; \
                                      \    return foo;}")

tFullClassCodeGen =
  TestCase (assertEqual "Method returning an int"
            (Just
             (joinTags
              "function Foo.baz 0 \
             \ push argument 0 \
             \ pop pointer 0 \
             \ push argument 1 \
             \ push constant 1 \
             \ add \
             \ pop static 0 \
             \ push constant 0 \
             \ return"))
            $ fmap joinTags
            $ evalVM
            $ tryParse classP " class Foo { \
                                \ static int blarg; \
                                \ field boolean bar; \
                                \ method void baz (int biz) \
                                \ { let blarg = biz + 1; \
                                \ return; }}")

tClassWithArrayHandling =
  TestCase (assertEqual "Class with array handling and method calls"
            (Just
             (joinTags
             "function Foo.baz 1 \
            \ push argument 0 \
            \ pop pointer 0 \
            \ push argument 1 \
            \ call Array.new 1 \
            \ pop local 0 \
            \ push constant 2 \
            \ push local 0 \
            \ add \
            \ push constant 5 \
            \ pop temp 0 \
            \ pop pointer 1 \
            \ push temp 0 \
            \ pop that 0 \
            \ push constant 0 \
            \ return \
            \ function Foo.bar 0 \
            \ push argument 0 \
            \ pop pointer 0 \
            \ push pointer 0 \
            \ push constant 5 \
            \ call Foo.baz 2 \
            \ pop temp 0 \
            \ push constant 7 \
            \ return"))
            $ fmap joinTags
            $ evalVM
            $ tryParse classP "class Foo { \
                                \ method void baz (int biz) \
                                \ { var Array x; \
                                \   let x = Array.new(biz); \
                                \   let x[2] = 5; \
                                \ return; } \
                                \ method int bar () \
                                \ { do baz(5); \
                                \   return 7;} \
                                \ }")

tClassWithStringConst =
  TestCase (assertEqual "Class with string const"
            (Just
             (joinTags
              "function Foo.boo 1 \
              \ push argument 0 \
              \ pop pointer 0 \
              \ push constant 4 \
              \ call String.new 1 \
              \ push constant 102 \
              \ call String.appendChar 2 \
              \ push constant 111 \
              \ call String.appendChar 2 \
              \ push constant 111 \
              \ call String.appendChar 2 \
              \ push constant 111 \
              \ call String.appendChar 2 \
              \ pop local 0 \
              \ push constant 0 \
              \ return"))
            $ fmap joinTags
            $ evalVM
            $ tryParse classP "class Foo { \
                              \ method void boo () \
                              \ {var str x; \
                              \  let x = \"fooo\"; \
                              \  return;}}")

tClassWithSubCallDiffClass =
  TestCase (assertEqual "Class with sub call from diff class"
            (Just
             (joinTags
             " function Foo.boo 1 \
               \ push argument 0 \
               \ pop pointer 0 \
               \ call Blarg.new 0 \
               \ pop local 0 \
               \ push local 0 \
               \ call Blarg.foo 1 \
               \ pop temp 0 \
               \ push constant 0 \
               \ return"))
            $ fmap joinTags
            $ evalVM
            $ tryParse classP "class Foo { \
                             \ method void boo () \
                             \ {var Blarg b1; \
                             \  let b1 = Blarg.new(); \
                             \  do b1.foo(); \
                             \  return;}} ")

exprTests =
  TestList [TestLabel "5 + 6 + 7" tSimpleExpr,
            TestLabel "(4 + 2) - 8 + (3 * (2 + 1))" tExprWithParens,
            TestLabel "x + g(2,y,-z) * 5" tExprWithMethodCall]

statementTests =
  TestList [TestLabel "let x = 5;" tLetStatementCG,
            TestLabel "let g = 3 + 5;" tLetStatementWExprCG,
            TestLabel "let y[0] = 3;" tLetStatementVarNameExprCG,
            TestLabel "do foo.bar(1, 2);" tDoSubCallStatementCG,
            TestLabel "return;" tReturnNoExprCG,
            TestLabel "return 5 * 5;" tReturnWithExprCG,
            TestLabel "if ((1 + 2) = 3) {let x = 6;}" tIfStatementNoElseCG,
            TestLabel "if with else" tIfStatementElseCG,
            TestLabel "{while (true) {do bar.baz();}}" tWhileStatementCG,
            TestLabel "Let with array access on right" tLetStatementWithArrayAccess,
            TestLabel "do call with a 'this' arg" tDoCallThisArg,
            TestLabel "do call pushes obj pointer before args" tDoCallPushesObjPointer]

symbolTableUpdateTests =
  TestList [TestLabel "single VarDec update" tSingleVarDecSymTableUpdate,
            TestLabel "param list update" tParamListSymTableUpdate]

subroutineDeclartaionTests =
  TestList [TestLabel "Simple void function" tSimpleVoidFunc,
            TestLabel "Simple Int return func" tSimpleIntReturnFunc,
            TestLabel "Constructor with one class field" tConstructor,
            TestLabel "Method returning an int" tMethod]

classLevelCodeGen =
  TestList [TestLabel "Full class code gen" tFullClassCodeGen,
            TestLabel "Class with array handling" tClassWithArrayHandling,
            TestLabel "Class with string const" tClassWithStringConst,
            TestLabel "Class with sub call from diff class" tClassWithSubCallDiffClass]

runVMGenTests :: Test
runVMGenTests =
  TestList
  $ concat $ [l | (TestList l) <- [exprTests,
                                   statementTests,
                                   symbolTableUpdateTests,
                                   subroutineDeclartaionTests,
                                   classLevelCodeGen]]
