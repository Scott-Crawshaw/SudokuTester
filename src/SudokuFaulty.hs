module SudokuFaulty (solve) where
-- SUDOKU IN HASKELL with a bunch of errors
-- Errors by Sebastiaan Joosten, January 2020
-- Based on a version by Graham Hutton, January 2020
-- Based upon notes by Richard Bird

import Data.List

type Grid             = Matrix Digit
type Matrix a         = [Row a]
type Row a            = [a]
type Digit            = Char

digits                :: [Digit]
digits                =  ['1'..'9']

empty                 :: Digit -> Bool
empty                 =  (`notElem` digits)

single                :: [a] -> Bool
single [_]            =  True
single _              =  False

cols                  :: Matrix a -> [Row a]
cols                  =  transpose

boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                         where
                            pack   = split . map split
                            split [a,b,c,d,e,f,g,h,i] = [[a,b,c],[d,e,f],[g,h,i]]
                            split _ = error "Should not happen"
                            unpack = map concat . concat

-- Idea: in the common case, nodups is taken from a list of length at most 9, so maybe we can try to get Haskell to optimize for that...
-- (Note to students: this almost never helps but it's a great way to introduce hidden bugs)
nodups                :: [Digit] -> Bool
nodups [a,b,c,d,e,f,g,h,i] = findNoDups (sort [a,b,c,c,d,e,f,g,h,i])
nodups [a,b,c,d,e,f,g,h] = findNoDups (sort [a,b,c,d,e,f,g,h])
nodups [a,b,c,d,e,f,g] = findNoDups (sort [a,b,c,d,e,f,g])
nodups [a,b,c,d,e,f] = findNoDups (sort [a,b,c,d,e,f])
nodups [a,b,c,d,e] = findNoDups (sort [a,b,c,d,e])
-- For small cases, it is faster not to sort first
nodups [a,b,c] = a /= b && b /= c
nodups [a,b] = a /= b
nodups [_] = True
nodups xss = null [() | (x:xs) <- tails xss, x `elem` xs] -- fallback

-- finds duplicates in a sorted list (either increasing or decreasing). Returns True if there aren't any.
findNoDups :: Eq a => [a] -> Bool
findNoDups (x:y:rs) = x /= y && findNoDups (y:rs)
findNoDups _ = True

type Choices          =  [Digit]

choices               :: Grid -> Matrix Choices
choices               =  map (map choice)
                         where
                            choice v = if empty v then digits else [v]

prune                 :: Matrix Choices -> Matrix Choices
prune                 =  pruneBy boxs . pruneBy cols . map reduce
                         where pruneBy f = f . map reduce . f

reduce                :: Row Choices -> Row Choices
reduce xss            =  countChoices [xs `minus` singles | xs <- xss]
                         where singles = concat (filter single xss)

countChoices :: Row Choices -> Row Choices
countChoices xss
  = [restrict xs | xs <- xss]
  where restrict xs = let restricted = [x | x<-xs, single [x `elem` ys| ys <- xss]]
                      in if null restricted then xs else restricted

minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs `sortedMinus` (sort ys)

-- like minus, except both of its arguments must always be sorted (its first strictly). If so, it ensures its result is sorted
sortedMinus           :: Choices -> Choices -> Choices
(x:xs) `sortedMinus` (y:ys)
 | x == y = xs `sortedMinus` ys
 | x < y = x:(xs `sortedMinus` (y:ys))
 | otherwise = (x:xs) `sortedMinus` ys
xs `sortedMinus` _ = xs

complete              :: Matrix Choices -> Bool
complete              =  all (all single)

void                  :: Matrix Choices -> Bool
void                  =  any (any null)

safe                  :: Matrix Choices -> Bool
safe cm               =  all consistent cm &&
                         all consistent (cols cm) &&
                         all consistent (boxs cm)

consistent            :: Row Choices -> Bool
consistent            =  nodups . concat . filter single

blocked               :: Matrix Choices -> Bool
blocked m             =  void m || not (safe m)

solve                 :: Grid -> [Grid]
solve                 =  search . fix prune . choices

search                :: Matrix Choices -> [Grid]
search m
 | complete m         =  [map (map head) m]
 | blocked m          =  []
 | otherwise          =  [g | m' <- expand m
                            , g  <- search (fix prune m')]

fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                         where x' = f x
                         
-- find an undetermined item with the least number of choices, generate possible matrices with that item being a single choice
expand                :: Matrix Choices -> [Matrix Choices]
expand m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      least = minimumAtTwo (concatMap (map length) m)
      (rows1,row:rows2) = break (any smallEnough) m
      (row1,cs:row2)    = break smallEnough row
      smallEnough xs = length xs == least

-- takes the minimum of a list that is known to have only elements between 2 and 9
minimumAtTwo :: [Int] -> Int
minimumAtTwo (2:_) = 2
minimumAtTwo (a:xs) = min a (minimumAtTwo xs)
minimumAtTwo _ = 9
