module TestHelpers where

import Data.List
import SudokuCorrect
import SudokuFaulty

superEasy :: Grid
superEasy =  ["2495716.8","861432975","57398.142","725698413","69814.257","314725869","9378..526","1..369784","486257391"]

checkAllGridsID :: (Grid -> Grid) -> Bool
checkAllGridsID f = (f easy == easy) && (f gentle == gentle) && (f diabolical == diabolical) && (f unsolvable == unsolvable) && (f minimal == minimal) && (f blank == blank)

--allTheSame function taken from https://stackoverflow.com/q/6121256
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

checkAllSolvers :: Grid -> Bool
checkAllSolvers g = allTheSame [SudokuCorrect.solve g, solve2 g, solve3 g, solve4 g, SudokuFaulty.solve g]

foldrID :: [Int] -> Bool
foldrID xs = (foldr (:) [] xs) == xs

filterFoldr :: (Int -> Bool) -> [Int] -> Bool
filterFoldr f xs = (foldr (\x -> if f x then (:) x else id) [] xs) == (filter f xs)

mnFunc :: Int -> Int -> Bool
mnFunc m n = ((m + 1) + n) == ((m + n) + 1)

sumFoldrTest :: [Int] -> Bool
sumFoldrTest xs = (sum xs) == (foldr (+) 0 xs)

xmFunc :: Int -> Int -> Bool
xmFunc x m = (x ^ (m + 1)) == (x * (x ^ m))

xyFunc :: Double -> Int -> Bool
xyFunc x y = (x ^^ y) == (x ^ y)