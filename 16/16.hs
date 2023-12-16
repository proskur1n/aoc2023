import Data.Array (Array, listArray, (!), bounds, inRange)
import qualified Data.Set as Set

type Position = (Int, Int) -- (y, x)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

bounce :: Char -> Direction -> [Direction]
bounce '/' North = [East]
bounce '/' East = [North]
bounce '/' South = [West]
bounce '/' West = [South]
bounce '\\' North = [West]
bounce '\\' East = [South]
bounce '\\' South = [East]
bounce '\\' West = [North]
bounce '|' East = [North, South]
bounce '|' West = [South, North]
bounce '-' North = [West, East]
bounce '-' South = [East, West]
bounce _ d = [d]

move :: Position -> Direction -> Position
move (y, x) North = (y - 1, x)
move (y, x) East = (y, x + 1)
move (y, x) South = (y + 1, x)
move (y, x) West = (y, x - 1)

energized :: Array Position Char -> Position -> Direction -> Int
energized grid pos dir = Set.size $ Set.map fst $ go pos dir Set.empty
  where
    go p d visited
      | not $ inRange (bounds grid) p = visited
      | (p, d) `Set.member` visited = visited
      | otherwise =
        let
          visited' = Set.insert (p, d) visited
          beams = bounce (grid ! p) d
        in
          foldl (\v d' -> go (move p d') d' v) visited' beams

-- Generic Depth-First-Search implementation. I didn't use it for this
-- solution, but it might be useful later.
--
-- dfs :: Ord a => (a -> [a]) -> a -> Set.Set a
-- dfs getChildren = dfs' Set.empty
--   where
--     dfs' visited node = foldl' dfs' visited' children'
--       where
--         visited' = Set.insert node visited
--         children' = filter (`Set.notMember` visited') (getChildren node)

parse :: String -> Array Position Char
parse input = listArray ((1, 1), (rows, cols)) $ concat lines'
  where
    lines' = lines input
    rows = length lines'
    cols = length $ head lines'

partOne :: Array Position Char -> Int
partOne grid = energized grid (1, 1) East

partTwo :: Array Position Char -> Int
partTwo grid = maximum $ map (uncurry $ energized grid) configurations
  where
    (rows, cols) = snd $ bounds grid
    configurations =
      [((1, x), South) | x <- [1..cols]]
      ++ [((rows, x), North) | x <- [1..cols]]
      ++ [((y, 1), East) | y <- [1..rows]]
      ++ [((y, cols), West) | y <- [1..rows]]

main :: IO ()
main = do
  grid <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne grid)
  putStrLn $ "Part Two: " ++ show (partTwo grid)
