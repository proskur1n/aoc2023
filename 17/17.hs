{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt)
import Control.Monad.State.Strict (State, evalState, get, gets, modify, put)
import Control.Monad (when, forM_)
import Data.Bifunctor (first, second)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int) -- (y, x)

data Dir = U | D | L | R deriving (Eq, Ord, Enum, Show)

type HeatLoss = Int

type Steps = Int

parse :: String -> Map Pos HeatLoss
parse input =
  Map.fromAscList
    [ ((y, x), digitToInt heat)
    | (y, line) <- zip [1..] (lines input)
    , (x, heat) <- zip [1..] line
    ]

move :: Pos -> Dir -> Pos
move (y, x) = \case
  U -> (y - 1, x)
  D -> (y + 1, x)
  L -> (y, x - 1)
  R -> (y, x + 1)

turnLeft :: Dir -> Dir
turnLeft = \case
  U -> L
  D -> R
  L -> D
  R -> U

turnRight :: Dir -> Dir
turnRight = \case
  U -> R
  D -> L
  L -> U
  R -> D

dijkstra :: Map Pos HeatLoss -> Steps -> Steps -> HeatLoss
dijkstra grid minStraight maxStraight = evalState loop (Map.empty, Set.fromList [(0, start, D), (0, start, R)])
  where
    (start, _) = Map.findMin grid
    (finish, _) = Map.findMax grid

    loop = do
      (heatLoss, pos, dir) <- popMinimum
      if pos == finish then
        return heatLoss
      else do
        let neighbors = concatMap (roll heatLoss pos) [turnLeft dir, turnRight dir]
        forM_ neighbors \(heatLoss', pos', dir') -> do
          let node = (pos', dir')
          currHeatLoss <- gets $ Map.findWithDefault maxBound node . fst
          when (heatLoss' < currHeatLoss) do
            modify $ first $ Map.insert node heatLoss'
            modify $ second $ Set.delete (currHeatLoss, pos', dir')
            modify $ second $ Set.insert (heatLoss', pos', dir')
        loop

    popMinimum = do
      (currentBest, queue) <- get
      let (elem, queue') = Set.deleteFindMin queue
      put (currentBest, queue')
      return elem

    roll :: HeatLoss -> Pos -> Dir -> [(HeatLoss, Pos, Dir)]
    roll heatLoss pos dir =
      let
        positions = takeWhile inside $ iterate (\p -> move p dir) pos
        weights = scanl1 (+) $ heatLoss : map (grid !) (tail positions)
      in
        take (maxStraight - minStraight + 1) $ drop minStraight $ zip3 weights positions (repeat dir)

    inside :: Pos -> Bool
    inside (y, x) =
      let
        (y0, x0) = start
        (y1, x1) = finish
      in
        y0 <= y && y <= y1 && x0 <= x && x <= x1

partOne :: Map Pos HeatLoss -> HeatLoss
partOne grid = dijkstra grid 1 3

partTwo :: Map Pos HeatLoss -> HeatLoss
partTwo grid = dijkstra grid 4 10

main :: IO ()
main = do
  grid <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne grid)
  putStrLn $ "Part Two: " ++ show (partTwo grid)
