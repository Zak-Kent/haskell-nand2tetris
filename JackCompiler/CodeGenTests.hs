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
  Just $ S.evalState (genVM vm) (classSymT, localSymT, 0)

checkSymTables ::  (VMGen a) => Maybe a -> Maybe (SymTable, SymTable)
checkSymTables Nothing = Nothing
checkSymTables (Just vm) =
    let (clsSyms, subSyms, _) = S.execState (genVM vm) (M.empty, M.empty, 0)
    in Just (clsSyms, subSyms)

tSimpleExpr = TestCase (assertEqual "5 + 6 + 7"
                        (Just
                         (joinTags
                          "push 5 \
                          \push 6 \
                          \push 7 \
                          \+ \
                          \+"))
                        $ fmap joinTags
                        $ evalVM
                        $ tryParse exprP "5 + 6 + 7")

tExprWithParens = TestCase (assertEqual "(4 + 2) - 8 + (3 * (2 + 1))"
                            (Just
                             (joinTags
                              "push 4 \
                             \ push 2 \
                             \ + \
                             \ push 8 \
                             \ push 3 \
                             \ push 2 \
                             \ push 1 \
                             \ + \
                             \ * \
                             \ + \
                             \ -"))
                            $ fmap joinTags
                            $ evalVM
                            $ tryParse exprP "(4 + 2) - 8 + (3 * (2 + 1))")

tExprWithMethodCall = TestCase (assertEqual "x + g(2,y,-z) * 5"
                                (Just
                                 (joinTags
                                  "push argument 1 \
                                 \ push 2 \
                                 \ push local 5 \
                                 \ push static 2 \
                                 \ - \
                                 \ call g \
                                 \ push 5 \
                                 \ * \
                                 \ +"))
                                $ fmap joinTags
                                $ evalVM
                                $ tryParse exprP "x + g(2,y,-z) * 5")

tLetStatementCG = TestCase (assertEqual "let x = 5;"
                            (Just
                             (joinTags
                              "push 5 \
                             \ pop \
                             \ argument 1"))
                           $ fmap joinTags
                           $ evalVM
                           $ tryParse statementP "let x = 5;")

tLetStatementWExprCG = TestCase (assertEqual "let g = 3 + 5;"
                                 (Just
                                  (joinTags
                                   "push 3 \
                                  \ push 5 \
                                  \ + \
                                  \ pop field 0"))
                           $ fmap joinTags
                           $ evalVM
                           $ tryParse statementP "let g = 3 + 5;")

tLetStatementVarNameExprCG =
  TestCase (assertEqual "let y[0] = 3;"
           -- TODO: this is behavior is wrong, fix when you learn how
           -- to handle array access
            (Just
             (joinTags
              "push 3 \
             \ pop \
             \ push 0 \
             \ pop local 5"))
             $ fmap joinTags
             $ evalVM
             $ tryParse statementP "let y[0] = 3;")

tDoSubCallStatementCG =
  TestCase (assertEqual "do foo.bar(1, 2);"
            (Just
             (joinTags
              "push 1 \
             \ push 2 \
             \ call foo.bar"))
             $ fmap joinTags
             $ evalVM
             $ tryParse statementP "do foo.bar(1, 2);")

tReturnNoExprCG =
  TestCase (assertEqual "return"
            (Just "return")
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "return;")

tReturnWithExprCG =
  TestCase (assertEqual "return 5 * 5;"
            (Just
             (joinTags
              "push 5 \
             \ push 5 \
             \ * \
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
              "push 1 \
             \ push 2 \
             \ + \
             \ push 3 \
             \ = \
             \ push not \
             \ if-goto L1 \
             \ push 6 \
             \ pop argument 1 \
             \ goto L2 \
             \ label L1 \
             \ label L2"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "if ((1 + 2) = 3) {let x = 6;}")

tIfStatementElseCG =
  TestCase (assertEqual "if (false) {let g = 2;} else {do foo.bar();} "
            (Just
             (joinTags
              "push false \
             \ push not \
             \ if-goto L1 \
             \ push 2 \
             \ pop field 0 \
             \ goto L2 \
             \ label L1 \
             \ call foo.bar \
             \ label L2"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "if (false) {let g = 2;} else {do foo.bar();} ")

tWhileStatementCG =
  TestCase (assertEqual "{while (true) {do bar.baz();}}"
            (Just
             (joinTags
              "label L1 \
             \ push true \
             \ push not \
             \ if-goto L2 \
             \ call bar.baz \
             \ goto L1 \
             \ label L2"))
            $ fmap joinTags
            $ evalVM
            $ tryParse statementP "while (true) {do bar.baz();}")

tSingleVarDecSymTableUpdate =
  TestCase (assertEqual "Single VarDec updates subroutine symbol table"
            (Just 2)
            $ fmap (\(clsS, subS) -> length $ M.keys subS)
            $ checkSymTables
            $ tryParse varDecP "var int foo, bar;")

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
            TestLabel "{while (true) {do bar.baz();}}" tWhileStatementCG]

symbolTableUpdateTests =
  TestList [TestLabel "single VarDec update" tSingleVarDecSymTableUpdate]

runVMGenTests :: Test
runVMGenTests =
  TestList
  $ concat $ [l | (TestList l) <- [exprTests,
                                   statementTests,
                                   symbolTableUpdateTests]]
