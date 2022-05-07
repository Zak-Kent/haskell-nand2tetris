import Test.HUnit

import CodeGen
-- import AST
import Parser
import XMLTests

tSimpleExpr = TestCase (assertEqual "5 + 6 + 7"
                        (Just
                         (joinTags
                          "push 5 \
                          \push 6 \
                          \push 7 \
                          \+ \
                          \+"))
                        $ fmap joinTags
                        $ fmap genVM
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
                            $ fmap genVM
                            $ tryParse exprP "(4 + 2) - 8 + (3 * (2 + 1))")

tExprWithMethodCall = TestCase (assertEqual "x + g(2,y,-z) * 5"
                                (Just
                                 (joinTags
                                  "push x \
                                 \ push 2 \
                                 \ push y \
                                 \ push z \
                                 \ - \
                                 \ call g \
                                 \ push 5 \
                                 \ * \
                                 \ +"))
                                $ fmap joinTags
                                $ fmap genVM
                                $ tryParse exprP "x + g(2,y,-z) * 5")

exprTests =
  TestList [TestLabel "5 + 6 + 7" tSimpleExpr,
            TestLabel "(4 + 2) - 8 + (3 * (2 + 1))" tExprWithParens,
            TestLabel "x + g(2,y,-z) * 5" tExprWithMethodCall]

runVMGenTests :: Test
runVMGenTests =
  TestList
  $ concat $ [l | (TestList l) <- [exprTests]]
