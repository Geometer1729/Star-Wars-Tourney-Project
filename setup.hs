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

main::IO ()
main = run (do
  args <- vc getArgs
  fighter1 <- readFighter (args!!0)
  vc  (putStrLn (show fighter1) ) )

readFighter :: [Char]-> CIO Fighter
readFighter name = do
  pat <- readGeneric name
  let tpat = Gr (trimGrid (gr pat))
  let h = length (gr tpat)
  let w = length ((gr tpat)!!0)
  p <- getPeriod pat
  return (Fighter tpat ((split '.' name)!!0) h w p )

getPeriod::Pattern -> CIO Int
getPeriod pat = do
  res<- (periodHelper pat 1)
  return res

periodHelper::Pattern -> Int -> CIO Int
periodHelper pat n = do
    thisOne <- testn pat n
    if thisOne then do
      return n
    else do
      out <- periodHelper pat (n+1)
      return out

testn::Pattern -> Int -> CIO Bool
testn pat n = do
  new <- (stepPattern pat n)
  return (new == pat)

stepPattern :: Pattern -> Int -> CIO Pattern
stepPattern pat1 n = do
  names <- vc (getName 2)
  vc ( writeFile (names!!0) ((rleHead 0 0 0 0) ++ (rle pat1)) )
  vc (callCommand (concat ["bgolly -q -q -a Generations -m ",show(n)," -o ",(names!!1)," ",(names!!0),">/dev/null"]) )
  pat2 <- readGeneric (names!!1)
  qc ( callCommand ("del "++(names!!0)) )
  qc ( callCommand ("del "++(names!!1)) )
  return pat2

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
