module Grid
(split
,squareup
,roundOut
,expand
,ceilLog2 -- so gollys can tell what size they are
,trimGrid
,joinGrids
,hspacer
,pushUp
,flipHz
)where

import Data.List

split:: (Eq a) => a -> [a] -> [[a]]
split s xs = splithelper s [] xs
  where
    splithelper:: (Eq a) => a -> [a] -> [a] -> [[a]]
    splithelper s temp (x:xs)
      | (s == x) = temp : (splithelper s [] xs)
      | otherwise = splithelper s (temp ++ [x] ) xs
    splithelper _ temp [] = [temp]

trimGrid::[[Int]] ->[[Int]] -- Removes rows of all zeros
trimGrid = trimVert . trimHorz . roundOut
  where
    trimHorz::[[Int]] ->[[Int]]
    trimHorz = transpose . trimVert . transpose
    trimVert::[[Int]] ->[[Int]]
    trimVert =   reverse . trimTop . reverse . trimTop
    trimTop::[[Int]] ->[[Int]]
    trimTop (x:xs)
      |isZeros x = trimTop xs
      |otherwise = (x:xs)
    trimTop [] = []
    isZeros::[Int] -> Bool
    isZeros xs = and [x == 0 | x <- xs ]

join2Grids::[[Int]] ->[[Int]] ->[[Int]]
join2Grids g1 g2
  | h1 > h2 = zipWith (++) g1 (extendg g2 (h1-h2))
  | otherwise = zipWith (++) (extendg g1 (h2-h1)) g2
  where
    h1 = length g1
    h2 = length g2

extendg::[[Int]] -> Int -> [[Int]] --pads a grid with n rows of zeros
extendg [] n = []
extendg g n = g ++ makeZeros (length (g!!0)) n

extendl::Int->[Int]->[Int]
extendl n xs = xs ++ (take (n - (length xs)) (cycle [0])) -- extends a line up to length n

makeZeros::Int->Int->[[Int]]
makeZeros x y =  [[0 | _ <- [1..x]] |  _ <- [1..y] ]

gridShow:: [[Int]] -> [Char] -- cleaner way to show large grids
gridShow dat = concat [ ((concat [(show_(x)++" ") | x <- row]) ++ "\n") | row <- dat  ]
  where
    show_ 0 = "_"
    show_ x = show(x)

hspacer::Int -> [[Int]]
hspacer n = [ (take n [0]) ]

pushUp::Int->[[Int]]->[[Int]]
pushUp n grid = reverse (extendg (reverse grid) n)

joinGrids::[[[Int]]] -> [[Int]]
joinGrids (g1:g2:gs) = joinGrids ((join2Grids g1 g2):gs)
joinGrids (g1:[]) = g1

expand::[[Int]] -> [[Int]] --brings grid size up to 2^n * 2^n for MC compresion
expand dat = squareup [dat,newRight,newBot,newCorn]
  where
    vNow = length dat
    hNow = length (dat!!0)
    maxLen = max hNow vNow
    minNewLength = next2 maxLen
    rightExp = (minNewLength - hNow)
    botExp = (minNewLength - vNow)
    newRight = makeZeros rightExp vNow
    newBot =  makeZeros hNow botExp
    newCorn = makeZeros rightExp botExp
    makeZeros::Int->Int->[[Int]]
    makeZeros x y =  [[0 | _ <- [1..x]] |  _ <- [1..y] ]

next2::Int->Int
next2 r = 2^(ceilLog2 r)

ceilLog2::Int->Int
ceilLog2 r = head [n | n <-[1..] , (2^n)>=r ]

roundOut::[[Int]]->[[Int]] -- adds zeros to short rows to fix RLE reads
roundOut dat = extended
  where
    newLen = maximum [length row | row <- dat]
    extended = [extendl newLen row | row <- dat]

squareup::[[[a]]] -> [[a]]
squareup (tl:tr:bl:br:[]) = (zipWith (++) tl tr) ++ (zipWith (++) bl br)

flipHz::[[Int]] -> [[Int]]
flipHz dat = [reverse row |  row <- dat]
