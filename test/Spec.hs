import Test.Tasty
import Test.Tasty.HUnit
import SudokuCorrect
import TestHelpers
import Test.Tasty.LeanCheck as LC

main:: IO ()
main = defaultMain (testGroup "Library Tests" [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10])
test1, test2, test3, test4, test5, test6, test7, test8, test9, test10 :: TestTree
test1 = localOption (Timeout 1000000"1 second") $
    testCase "Rows function is its own inverse" (assertBool "rows function is not its own inverse" (checkAllGridsID (rows.rows)))
test2 = localOption (Timeout 1000000"1 second") $
    testCase "Cols function is its own inverse" (assertBool "cols function is not its own inverse" (checkAllGridsID (cols.cols)))
test3 = localOption (Timeout 1000000"1 second") $
    testCase "Box function is its own inverse" (assertBool "boxs function is not its own inverse" (checkAllGridsID (boxs.boxs)))
test4 = localOption (Timeout 10000000"10 seconds") $
    testCase "All 5 solve functions produce same result" (assertBool "Not all solve functions produced same result" (checkAllSolvers superEasy))
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