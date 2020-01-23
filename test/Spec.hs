import Test.Tasty
import Test.Tasty.HUnit
import SudokuCorrect
import SudokuFaulty
import TestHelpers
import Test.Tasty.LeanCheck as LC

main:: IO ()
main = defaultMain (testGroup "Library Tests" [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24, test25, test26, test27, test28, test29, test30, test31])
test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24, test25, test26, test27, test28, test29, test30, test31 :: TestTree
test1 = localOption (Timeout 5000000"5 seconds") $
    testCase "Rows function is its own inverse" (assertBool "rows function is not its own inverse" (checkAllGridsID (rows.rows)))
test2 = localOption (Timeout 5000000"5 seconds") $
    testCase "Cols function is its own inverse" (assertBool "cols function is not its own inverse" (checkAllGridsID ((SudokuCorrect.cols).(SudokuCorrect.cols))))
test3 = localOption (Timeout 5000000"5 seconds") $
    testCase "Box function is its own inverse" (assertBool "boxs function is not its own inverse" (checkAllGridsID ((SudokuCorrect.boxs).(SudokuCorrect.boxs))))
test4 = localOption (Timeout 5000000"5 seconds") $
    testCase "All 5 solve functions produce same result on trivial board" (assertBool "Not all solve functions produced same result" (checkAllSolvers superEasy))
test11 = localOption (Timeout 5000000"5 seconds") $
    testCase "SudokuCorrect.solve on easy board" (assertBool "Invalid Solution" (valid ((SudokuCorrect.solve easy)!!0)))
test12 = localOption (Timeout 5000000"5 seconds") $
    testCase "SudokuCorrect.solve2 on easy board" (assertBool "Invalid Solution" (valid ((SudokuCorrect.solve2 easy)!!0)))
test13 = localOption (Timeout 5000000"5 seconds") $
    testCase "SudokuFaulty.solve on easy board" (assertBool "Invalid Solution" (valid ((SudokuFaulty.solve easy)!!0)))
test14 = localOption (Timeout 5000000"5 seconds") $
    testCase "SudokuCorrect.solve3 on easy board" (assertBool "Invalid Solution" (valid ((SudokuCorrect.solve3 easy)!!0)))
test15 = localOption (Timeout 5000000"5 seconds") $
    testCase "SudokuCorrect.solve4 on easy board" (assertBool "Invalid Solution" (valid ((SudokuCorrect.solve4 easy)!!0)))
test16 = localOption (Timeout 5000000"5 seconds") $
    testCase "SudokuFaulty.nodups on list '121'" (assertBool "nodups returned True, yet 1 appears twice" (not(SudokuFaulty.nodups "121")))
test17 = LC.testProperty "The empty method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.empty (x::Digit)) == (SudokuFaulty.empty (x::Digit)))
test18 = LC.testProperty "The single method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.single (x::[Digit])) == (SudokuFaulty.single (x::[Digit])))
test19 = LC.testProperty "The cols method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.cols (x::Grid)) == (SudokuFaulty.cols (x::Grid)))
test20 = LC.testProperty "The boxs method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.boxs (x::Grid)) == (SudokuFaulty.boxs (x::Grid)))
test21 = LC.testProperty "The nodups method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.nodups (x::[Digit])) == (SudokuFaulty.nodups (x::[Digit])))
test22 = LC.testProperty "The choices method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.choices (x::Grid)) == (SudokuFaulty.choices (x::Grid)))
test23 = localOption (Timeout 5000000"5 seconds") $
    testCase "The prune method in SudokuCorrect and SudokuFaulty are equivalent" (assertBool "The prune methods did not return the same value" (((SudokuCorrect.prune (SudokuCorrect.choices easy)) == (SudokuFaulty.prune (SudokuCorrect.choices easy))) && ((SudokuCorrect.prune (SudokuCorrect.choices unsolvable)) == (SudokuFaulty.prune (SudokuCorrect.choices unsolvable)))))
test24 = LC.testProperty "The reduce method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.reduce (x::[[Digit]])) == (SudokuFaulty.reduce (x::[[Digit]])))
test25 = LC.testProperty "The minus method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x y -> (SudokuCorrect.minus (x::[Digit]) (y::[Digit])) == (SudokuFaulty.minus (x::[Digit]) (y::[Digit])))
test26 = localOption (Timeout 5000000"5 seconds") $
    testCase "The fix method in SudokuCorrect and SudokuFaulty are equivalent" (assertBool "The fix methods did not return the same value" (((SudokuCorrect.fix SudokuCorrect.prune (SudokuCorrect.choices easy)) == (SudokuFaulty.fix SudokuCorrect.prune (SudokuCorrect.choices easy))) && ((SudokuCorrect.fix SudokuCorrect.prune (SudokuCorrect.choices unsolvable)) == (SudokuFaulty.fix SudokuCorrect.prune (SudokuCorrect.choices unsolvable)))))
test27 = LC.testProperty "The complete method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.complete (x::[[[Digit]]])) == (SudokuFaulty.complete (x::[[[Digit]]])))
test28 = LC.testProperty "The consistent method in SudokuCorrect and SudokuFaulty are equivalent"
    (\x -> (SudokuCorrect.consistent (x::[[Digit]])) == (SudokuFaulty.consistent (x::[[Digit]])))
test29 = localOption (Timeout 5000000"5 seconds") $
    testCase "The blocked method in SudokuCorrect and SudokuFaulty are equivalent" (assertBool "The blocked methods did not return the same value" (((SudokuCorrect.blocked (SudokuCorrect.prune (SudokuCorrect.choices easy))) == (SudokuFaulty.blocked (SudokuCorrect.prune (SudokuCorrect.choices easy)))) && ((SudokuCorrect.blocked (SudokuCorrect.prune (SudokuCorrect.choices unsolvable))) == (SudokuFaulty.blocked (SudokuCorrect.prune (SudokuCorrect.choices unsolvable))))))
test30 = localOption (Timeout 5000000"5 seconds") $
    testCase "The search method in SudokuCorrect and SudokuFaulty are equivalent" (assertBool "The search methods did not return the same value" (((SudokuCorrect.search (SudokuCorrect.prune (SudokuCorrect.choices easy))) == (SudokuFaulty.search (SudokuCorrect.prune (SudokuCorrect.choices easy)))) && ((SudokuCorrect.search (SudokuCorrect.prune (SudokuCorrect.choices superEasy))) == (SudokuFaulty.search (SudokuCorrect.prune (SudokuCorrect.choices superEasy))))))
test31 = localOption (Timeout 5000000"5 seconds") $
    testCase "The expand method in SudokuCorrect and SudokuFaulty are equivalent" (assertBool "The expand methods did not return the same value" (((SudokuCorrect.expand (SudokuCorrect.prune (SudokuCorrect.choices easy))) == (SudokuFaulty.expand (SudokuCorrect.prune (SudokuCorrect.choices easy)))) && ((SudokuCorrect.expand (SudokuCorrect.prune (SudokuCorrect.choices unsolvable))) == (SudokuFaulty.expand (SudokuCorrect.prune (SudokuCorrect.choices unsolvable))))))
test5 = LC.testProperty "id = foldr (:) []"
    (\xs -> foldrID xs)
test6 = LC.testProperty "filter p = foldr (\\x -> if p x then (:) x else id) []"
    (\xs -> filterFoldr odd xs && filterFoldr even xs && filterFoldr (<25) xs && filterFoldr (>0) xs)
test7 = LC.testProperty "(m + 1) + n = (m + n) + 1"
    (\m n -> mnFunc m n)
test8 = LC.testProperty "sum = foldr (+) 0"
    (\xs -> sumFoldrTest xs)
test9 = LC.testProperty "x ^ (m + 1) = x * (x ^ m)"
    (\x m -> xmFunc x m)
test10 = LC.testProperty "x ^^ y = x ^ y"
    (\x y -> xyFunc x y)