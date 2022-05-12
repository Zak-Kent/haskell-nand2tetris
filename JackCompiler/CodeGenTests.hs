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

evalVM :: Maybe Expr -> Maybe String
evalVM Nothing = Nothing
evalVM (Just expr) =
  Just $ S.evalState (genVM expr) (classSymT, localSymT)

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
                                 \ call field 0 \
                                 \ push 5 \
                                 \ * \
                                 \ +"))
                                $ fmap joinTags
                                $ evalVM
                                $ tryParse exprP "x + g(2,y,-z) * 5")

exprTests =
  TestList [TestLabel "5 + 6 + 7" tSimpleExpr,
            TestLabel "(4 + 2) - 8 + (3 * (2 + 1))" tExprWithParens,
            TestLabel "x + g(2,y,-z) * 5" tExprWithMethodCall]

runVMGenTests :: Test
runVMGenTests =
  TestList
  $ concat $ [l | (TestList l) <- [exprTests]]
