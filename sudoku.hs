import Data.List (transpose, (\\))

type Matrix a = [Row a]
type Row a    = [a]
type Grid     = Matrix Digit
type Digit    = Char
type Choices  = [Digit]

solve :: Grid -> [Grid]
solve = filter valid . expand . choices

choices :: Grid -> Matrix Choices
choices = map (map choice)

expand :: Matrix Choices -> [Grid]
expand = sequenceA . map sequenceA

valid :: Grid -> Bool
valid g =
     all nodups (rows g)
  && all nodups (cols g)
  && all nodups (boxes g)

nodups :: [Digit] -> Bool
nodups [] = True
nodups (x:xs) = x `notElem` xs && nodups xs

rows :: [[a]] -> [[a]]
rows = id

cols :: [[a]] -> [[a]]
cols = transpose

boxes :: [[a]] -> [[a]]
boxes = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group xs = (take 3 xs) : (group . drop 3) xs

ungroup :: [[a]] -> [a]
ungroup = concat

choice :: Digit -> Choices
choice d = if blank d then digits else [d]

blank :: Digit -> Bool
blank = (== '0')

digits :: [Digit]
digits = ['1'..'9']

t1 = solve ["023", "450", "071"]

-----------

remove :: Eq a => [a] -> [a] -> [a]
remove _  xs@[x]  = xs
remove ds xs      = xs \\ ds

fixed :: [[a]] -> [a]
fixed row = [x | [x] <- row]

pruneRow :: Eq a => [[a]] -> [[a]]
pruneRow row = map (remove (fixed row)) row

pruneBy :: Eq a => ([[[a]]] -> [[[a]]]) -> [[[a]]] -> [[[a]]]
pruneBy f = f . map pruneRow . f

prune :: Eq a => [[[a]]] -> [[[a]]]
prune = pruneBy rows . pruneBy cols . pruneBy boxes

solve2 :: Grid -> [Grid]
solve2 = filter valid . expand . prune . choices

t2 = solve2 ["023", "450", "071"]

-----------

expand1 :: Matrix Choices -> [Matrix Choices]
expand1 rows = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where (rows1, row : rows2) = break (any smallest) rows
        (row1, cs : row2)    = break smallest row
        smallest cs          = length cs == n
        n                    = minimum (counts rows)

counts :: [[[a]]]  -> [Int]
counts = filter (/=1) . map length . concat

complete :: [[[a]]] -> Bool
complete = all (all single)

single :: [a] -> Bool
single [x] = True
single  _  = False

ok :: [[Digit]] -> Bool
ok = nodups . fixed

safe :: Matrix Choices -> Bool
safe m = all ok (rows m) && all ok (cols m) && all ok (boxes m)

search :: Matrix Choices -> [Grid]
search m | not (safe m) = []
         | complete m'  = [map (map head) m']
         | otherwise    = concat (map search (expand1 m'))
  where m' = prune m

solve3 :: Grid -> [Grid]
solve3 = search . choices

t3 = solve3 ["023", "450", "071"]

-- http://www.247sudoku.com/

hard =
  [ "400000600"
  , "800001000"
  , "000395007"
  , "201000360"
  , "000000024"
  , "037000000"
  , "090758000"
  , "000009100"
  , "300000400"
  ]

expert =
  [ "000000000"
  , "904063000"
  , "000094701"
  , "250000900"
  , "006005000"
  , "400810000"
  , "000502004"
  , "008000067"
  , "000030800"
  ]

-- https://wiki.haskell.org/Sudoku

nefarious =
  [ "000060080"
  , "020000000"
  , "001000000"
  , "070000102"
  , "500030000"
  , "000000400"
  , "004201000"
  , "300700600"
  , "000000050"
  ]
