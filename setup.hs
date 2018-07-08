import GollyFiles
import Data.List
import System.IO
import Control.Monad
import System.Environment
import System.Directory
import System.Process


data Fighter = Fighter {
 cannon :: Pattern
,name :: [Char]
,h :: Int
,w :: Int
,p :: Int
} deriving(Show)
data Battle = Battle {
 zone :: Pattern
,fighterl :: Fighter
,fighter2 :: Fighter
,x::Int
,y::Int
}

doAll::[IO()] -> IO ()
doAll = foldl (>>) (return())


main::IO ()
main = do
  args <- getArgs
  (fighter1,clean1) <- readFighter (args!!0)
  putStrLn (show fighter1)
  doAll clean1



readFighter :: [Char]->IO (Fighter, [IO ()] )
readFighter name = do
  (pat,close) <- readGeneric name
  let tpat = Gr (trimGrid (gr pat))
  let h = length (gr tpat)
  let w = length ((gr tpat)!!0)
  (p,periodClean) <- getPeriod pat
  return (  (Fighter tpat ((split '.' name)!!0) h w p ) , close:periodClean )

getPeriod::Pattern -> IO (Int,[IO()])
getPeriod pat = do
  (res,clean) <- (periodHelper pat (1,[]))
  return (res,clean)

periodHelper::Pattern -> (Int,[IO ()]) -> IO (Int,[IO ()])
periodHelper pat (n,clean_) = do
  (thisOne,clean) <- testn pat n
  if thisOne then do
    return (n,clean++clean_)
  else do
    out <- (periodHelper pat (n+1,clean++clean_) )
    return out

testn::Pattern -> Int -> IO (Bool,[IO()])
testn pat n = do
  (new,clean) <- (stepPattern pat n)
  return ((new == pat) , clean)



stepPattern :: Pattern -> Int -> IO (Pattern,[IO ()])
stepPattern pat1 n = do
  names <- (getName 2)
  writeFile (names!!0) (rleHead ++ (rle pat1))
  callCommand (concat ["bgolly -q -q -a Generations -m ",show(n)," -o ",(names!!1)," ",(names!!0)])
  (pat2,close) <- readGeneric (names!!1)
  let rm1 = callCommand ("del "++(names!!0))
  let rm2 = callCommand ("del  "++(names!!1))
  return (pat2,[close,rm1,rm2])

getName:: Int -> IO [[Char]]
getName count = do
  files <- getDirectoryContents "."
  let candNames = ["temp"++(show(n)++".rle") | n <- [0..]] :: [[Char]]
  let valNames = [nm | nm <- candNames , not (elem nm files)] :: [[Char]]
  return (take count valNames)



--bgolly -a Generations -m 10 -o test2.rle demoCannon.rle



split:: (Eq a) => a -> [a] -> [[a]]
split s xs = splithelper s [] xs
  where
    splithelper:: (Eq a) => a -> [a] -> [a] -> [[a]]
    splithelper s temp (x:xs)
      | (s == x) = temp : (splithelper s [] xs)
      | otherwise = splithelper s (temp ++ [x] ) xs
    splithelper _ temp [] = [temp]

trimGrid::[[Int]] ->[[Int]]
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
  | h1 > h2 = zipWith (++) g1 (extend g2 (h1-h2))
  | otherwise = zipWith (++) (extend g1 (h2-h1)) g2
  where
    h1 = length g1
    h2 = length g2
extend::[[Int]] -> Int -> [[Int]]
extend [] n = []
extend g n = g ++ makeZeros (length (g!!0)) n
makeZeros::Int->Int->[[Int]]
makeZeros x y =  [[0 | _ <- [1..x]] |  _ <- [1..y] ]

gridShow:: [[Int]] -> [Char]
gridShow dat = concat [ ((concat [(show_(x)++" ") | x <- row]) ++ "\n") | row <- dat  ]
  where
    show_ 0 = "_"
    show_ x = show(x)

flipHz:: Pattern -> Pattern
flipHz pat = Go (flipGhz (go pat))

flipGhz::Golly -> Golly
flipGhz (Empty) = Empty
flipGhz (Block 1 (a:b:c:d:[])) = Block 1 [b,a,d,c]
flipGhz (Node  n (a:b:c:d:[])) = Node  n (fmap flipGhz [b,a,d,c] )
flipGhz x = error (show (x == Empty))

hspacer::Int -> [[Int]]
hspacer n = [ (take n [0]) ]

pushUp::Int->[[Int]]->[[Int]]
pushUp n grid = reverse (extend (reverse grid) n)

joinGrids::[[[Int]]] -> [[Int]]
joinGrids (g1:g2:gs) = joinGrids ((join2Grids g1 g2):gs)
joinGrids (g1:[]) = g1

battlePatterns::(Pattern,Pattern) -> [Pattern]
battlePatterns (left,right) = concat [ [ Gr (joinGrids [(gr l),spacer,(gr r)]) | spacer <- spacers  ] | (l,r) <- boostlist] :: [Pattern]
  where
    rightg = gr right ::[[Int]]
    leftg = gr left ::[[Int]]
    hr = length rightg ::Int
    hl = length leftg :: Int
    right_ = flipHz right :: Pattern
    boostlist = boosts (left,right) :: [(Pattern,Pattern)]
    spacers = [hspacer n | n <- [20..30]] :: [[[Int]]]

boosts::(Pattern,Pattern) ->  [(Pattern,Pattern)]
boosts (l,r)
  |hl > hr = zip (cycle [l]) [Gr (pushUp n gR) | n <- [0..(hr-hl)]]
  |otherwise = take (hl-hr+1) (zip [Gr (pushUp n gL) | n <- [0..(hr-hl)]] (cycle [r]))
  where
    gR = gr r ::[[Int]]
    gL = gr l ::[[Int]]
    hr = length gR ::Int
    hl = length gL ::Int





{-
main::IO ()
main = do
    args <- getArgs
    (pat1,close1) <- readGeneric (args!!0)
    (pat2,close2) <- readGeneric (args!!1)
    let list = battlePatterns (pat1,pat2)
    foldl (>>) (return ()) [writeFile (("battles/test"++show(ind))++".rle") (rleHead ++ (rle pat)) | (pat,ind) <-(zip list [0..]) ]
    close1
    close2
-}
