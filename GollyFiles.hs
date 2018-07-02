module GollyFiles
(Golly(Node,Block,Empty)
,Pattern(MC,Dat,Go,Gr,Rle)
,mc
,dat
,go
,gr
,rle
,readGeneric
,rleHead
,mcHead
,roundOut
) where

import System.IO
import Data.Char
import Data.List
import Control.Monad
import Debug.Trace

traceThis:: (Show a) => a -> a
traceThis x = trace (show x) x



readGeneric::[Char]-> IO (Pattern , IO() )
readGeneric name = do
  handle <- openFile name ReadMode
  contents <- hGetContents handle
  let fileType= last (split '.' name)
  if fileType == "rle" then return ((Rle contents) , hClose handle)
  else if fileType == "mc" then return ((MC contents) , hClose handle)
  else error "File type not supported"


data Pattern = MC [Char] | Dat [[Int]] | Go Golly | Gr [[Int]] | Rle [Char]

instance Show Pattern where
  show pat = matShow (gr pat)

mc::Pattern-> [Char]
mc (MC x)  = x
mc (Dat x) = dataToMC x
mc (Go x)  = (dataToMC . gollyToData) x
mc (Gr x)  = (dataToMC . gollyToData . gridToGolly) x
mc (Rle x) = (dataToMC . gollyToData . gridToGolly . rleToGrid) x


dat::Pattern -> [[Int]]
dat (MC x)  = mcToData x
dat (Dat x) = x
dat (Go x)  = (gollyToData) x
dat (Gr x)  = (gollyToData . gridToGolly) x
dat (Rle x) = (gollyToData . gridToGolly . rleToGrid) x


go::Pattern -> Golly
go (MC x)  = (dataToGolly . mcToData) x
go (Dat x) = (dataToGolly) x
go (Go x)  = x
go (Gr x)  = (gridToGolly) x
go (Rle x) = (gridToGolly . rleToGrid) x


gr::Pattern -> [[Int]]
gr (MC x)  = (gollyToGrid . dataToGolly . mcToData) x
gr (Dat x) = (gollyToGrid . dataToGolly) x
gr (Go x)  = (gollyToGrid) x
gr (Gr x)  = x
gr (Rle x) = (rleToGrid) x


rle::Pattern->[Char]
rle (MC x)  = (gridToRle . gollyToGrid . dataToGolly . mcToData) x
rle (Dat x) = (gridToRle . gollyToGrid . dataToGolly) x
rle (Go x)  = (gridToRle . gollyToGrid) x
rle (Gr x)  = (gridToRle) x
rle (Rle x) = x
data Golly = Node Int [Golly] | Block Int [Int] | Empty deriving(Eq)

instance Show Golly where
  show t = shw 0 t

instance Eq Pattern where
  (==) pat1 pat2 = (==) (gr pat1) (gr pat2)

mcHead::[Char]
mcHead = "[M2] (golly 3.1)\n#R 345/2/4\n"

rleHead::[Char]
rleHead = "#CXRLE Pos=-20,-11\nx = 0, y = 0, rule = 345/2/4\n"

shw:: Int -> Golly -> [Char]
shw depth (Block d b) = concat ["\n",(repList "    " depth),"B ",show(b)," ",show(d)]
shw depth (Node d gs) = concat (["\n",(repList "    " depth),"N ",show(d)]  ++ [shw (depth+1) g | g<-gs])
shw depth (Empty) = concat ["\n",(repList "    " depth),"E"]

repList::[a] -> Int -> [a]
repList xs 0 = []
repList xs n = xs ++ (repList xs (n-1))

mcToData::[Char] -> [[Int]]
mcToData txt = dat
  where
    allLines = (split ('\n') txt)
    lines_ =  drop 2  allLines
    dat = [[ (read x :: Int) | x <- (split ' ' row)] | row <- (take ((length lines_)-1) lines_)]

split:: (Eq a) => a -> [a] -> [[a]]
split s xs = splithelper s [] xs
  where
    splithelper:: (Eq a) => a -> [a] -> [a] -> [[a]]
    splithelper s temp (x:xs)
      | (s == x) = temp : (splithelper s [] xs)
      | otherwise = splithelper s (temp ++ [x] ) xs
    splithelper _ temp [] = [temp]

dataToGolly:: [[Int]] -> Golly
dataToGolly rows = last (buildList rows [] )
  where
    buildList:: [[Int]] -> [Golly] -> [Golly]
    buildList [] context = context
    buildList (row:dat) context =  new : (buildList dat (context ++ [new]))
      where
        new = (helper row context)
    helper::[Int] -> [Golly] -> Golly
    helper (1:cels) context = Block 1 cels
    helper (n:blocks) context = Node n [ getBlock ind context | ind<-blocks]
      where
        getBlock:: Int -> [Golly] -> Golly
        getBlock 0 _ = Empty
        getBlock n gs
          | ((n-1) < length gs ) = gs !! (n-1)
          | otherwise = error "hek it all to gosh"
gollyToGrid::Golly->[[Int]]
gollyToGrid (Block 1 (tl:tr:bl:br:[])) = [[tl,tr],[bl,br]]
gollyToGrid (Node d gs) = squareup [depthAware g (d-1) | g <- gs]

depthAware:: Golly -> Int -> [[Int]]
depthAware (Node d gs) _ = squareup [depthAware g (d-1) | g <- gs]
depthAware (Block 1 (tl:tr:bl:br:[])) _ = [[tl,tr],[bl,br]]
depthAware Empty d = [[0 | _ <- sublist ] |  _ <- sublist ]
  where
    sublist = [1..(2^d)]

squareup::[[[a]]] -> [[a]]
squareup (tl:tr:bl:br:[]) = (zipWith (++) tl tr) ++ (zipWith (++) bl br)

expand::[[Int]] -> [[Int]]
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

gridToGolly::[[Int]] -> Golly --doesn't work for non square grids WTF!!!!
gridToGolly dat = helper (expand (roundOut dat))
  where
    helper::[[Int]] -> Golly
    helper dat
      | allZeros = Empty
      | ((length dat) == 2) = Block 1 (concat dat)
      | otherwise = Node d [helper sub | sub <- subs]
        where
          allZeros = (maximum [maximum row | row <- dat] == 0)
          d = ceilLog2 (length dat)
          subDim = 2^(d-1)
          subs = [tl,tr,bl,br]
          corner:: ([[a]]->[[a]]) -> ([a]->[a]) ->  [[a]] -> [[a]]
          corner t1 t2 dat = t1 [t2 row | row <- dat]
          fr = take subDim
          ls = drop subDim
          tl = corner fr fr dat
          tr = corner fr ls dat
          bl = corner ls fr dat
          br = corner ls ls dat

next2::Int->Int
next2 r = 2^(ceilLog2 r)

ceilLog2::Int->Int
ceilLog2 r = head [n | n <-[1..] , (2^n)>=r ]

gollyToData::Golly->[[Int]]
gollyToData g = [snd ent | ent <- allEnts ]
  where
    allEnts = (include g [])
    name :: Golly -> [Golly] -> Int
    name Empty _ = 0
    name g (g_:gs)
      | g == g_ = 1
      | otherwise = 1 + (name g gs)
    include::Golly -> [(Golly,[Int])] -> [(Golly,[Int])]
    include Empty gs = gs
    include g ents
      | (isPresent g ents) = ents
      | otherwise = addEnt g ents
      where
        isPresent:: Golly -> [(Golly,[Int])] -> Bool
        isPresent g ((g_,ent):gs)
          | g == g_ = True
          | otherwise = isPresent g gs
        isPresent _ [] = False
    addEnt::Golly -> [(Golly,[Int])] -> [(Golly,[Int])]
    addEnt Empty gs = gs
    addEnt (Block 1 cs) gs = gs++ [ ( (Block 1 cs) , 1:cs ) ]
    addEnt (Node d igs) gs =  newList ++ [ ( (Node d igs) , d:[name g newGollys | g <- igs] ) ]
      where
        newList = foldl (flip include) gs igs :: [(Golly,[Int])]
        newGollys = [fst n | n <- newList]

matShow:: [[Int]] -> [Char]
matShow dat = concat [ ((concat [(show(x)++" ") | x <- row]) ++ "\n") | row <- dat  ]

dataToMC::[[Int]]->[Char]
dataToMC dat = (matShow dat)

gridToRle::[[Int]]->[Char]
gridToRle dat = comp
  where
    comp = short tal
    tal = tally uncomp
    uncomp = concat [ [ch  x | x<- row] ++"$" | row <- dat] ++['!']

tally::(Eq a) => [a]->[(Int,a)]
tally [] = []
tally (x:xs) = rollingTally xs (1,x)

rollingTally::(Eq a) => [a] -> (Int,a) -> [(Int,a)]
rollingTally (x:xs) (n,c)
  | x == c = rollingTally xs (n+1,c)
  | otherwise = (n,c): ( rollingTally xs (1,x) )
rollingTally [] x = [x]

short:: [(Int,Char)] -> [Char]
short ((n,c):ts)
  | n==1 = c:(short ts)
  | otherwise = concat [show(n),[c],short ts]
short [] = []

ch::Int->Char
ch 0 = '.'
ch x = chr (x + 64)

rleToGrid::[Char] -> [[Int]]
rleToGrid str = roundOut grid
  where
    flines = split '\n' str
    unComp = unTally (concat [line | line<- flines , ( ((length line) == 0 ) || (not (elem (line!!0) "x#")))])
    lines_ = (split '$' (take ((length unComp)-1) unComp) )
    grid = [ [ od x | x<-line] | line<-lines_]

unTally::[Char]->[Char]
unTally (c:cs)
  | not (isDigit c) = c:(unTally cs)
  |otherwise = dnum [c] cs
unTally [] = []

dnum::[Char] -> [Char] -> [Char]
dnum num (c:cs)
  |isDigit c = dnum (num++[c]) cs
  |otherwise = (take (read num :: Int) (cycle [c])) ++ (unTally cs)

roundOut::[[Int]]->[[Int]]
roundOut dat = extended
  where
    newLen = maximum [length row | row <- dat]
    extended = [extend newLen row | row <- dat]

extend::Int->[Int]->[Int]
extend n xs = xs ++ (take (n - (length xs)) (cycle [0]))

od::Char->Int
od '.' = 0
od c = (ord c) -64
