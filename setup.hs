import GollyFiles
import Data.List
import System.IO
import Control.Monad
import System.Environment
import System.Directory
import System.Process
import Grid
import CIO

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
main = run (do
  args <- vc getArgs
  fighter1 <- readFighter (args!!0)
  vc  (putStrLn (show fighter1) ) )

readFighter :: [Char]-> CIO Fighter
readFighter name = C (do
  (pat,close) <- readGeneric name
  let tpat = Gr (trimGrid (gr pat))
  let h = length (gr tpat)
  let w = length ((gr tpat)!!0)
  (p,periodClean) <- getPeriod pat
  return (  (Fighter tpat ((split '.' name)!!0) h w p ) , close:periodClean ) )

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

battlePatterns::(Pattern,Pattern) -> [Pattern]
battlePatterns (left,right) = concat [ [ Gr (joinGrids [(gr l),spacer,(gr r)]) | spacer <- spacers  ] | (l,r) <- boostlist] :: [Pattern]
  where
    rightg = gr right ::[[Int]]
    leftg = gr left ::[[Int]]
    hr = length rightg ::Int
    hl = length leftg :: Int
    right_ = (patify flipHz) right :: Pattern
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
