{-# LANGUAGE ViewPatterns #-}
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find, intercalate)

type Pos = (Int, Int) -- (y, x)
type Garden = Array Pos Char

wrapAround :: Garden -> Pos -> Pos
wrapAround (bounds -> ((0, 0), (ym, xm))) (y, x) =
  (y `mod` (ym + 1), x `mod` (xm + 1))

bfs :: Garden -> Pos -> [[Pos]]
bfs garden start = go Set.empty [start]
  where
    go visited xs = Set.toList unvisited : go visited' next
      where
        unvisited = Set.fromList $ filter (`Set.notMember` visited) xs
        visited' = visited `Set.union` unvisited

        next =
          [ pos
          | (y, x) <- Set.toList unvisited, (dy, dx) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
          , let pos = (y + dy, x + dx)
          , walkable pos
          ]

    walkable (wrapAround garden -> pos) = garden!pos == '.'

reachablePositions :: Int -> Garden -> Pos -> [Pos]
reachablePositions steps garden start =
  concatMap snd $ filter (\(i, _) -> even (steps - i)) $ zip [0..steps] $ bfs garden start

parse :: String -> (Garden, Pos)
parse input = (garden // [(start, '.')], start)
  where
    lines' = lines input
    rows = length lines'
    cols = length $ head lines'
    garden = listArray ((0, 0), (rows-1, cols-1)) $ concat lines'
    Just (start, _) = find ((== 'S') . snd) $ assocs garden

-- Answer: 3642
partOne :: Garden -> Pos -> Int
partOne garden start = length $ reachablePositions 64 garden start

-- Answer: 608603023105276
partTwo :: Garden -> Int
partTwo garden = (q-1)*(q-1)*evenFull + q*q*oddFull + q*trigs + (q-1)*pentas + spikes
  where
    -- The second part requires hard-coding to this particular input data.
    -- I don't think there exists an efficient solution for a general case.
    steps = 26501365 -- (202300 * 131 + 65)
    q = 202300
    fields = reachablePositions (2 * 131 + 65) garden (65, 65)

    evenFull = memo!(0, 0)
    oddFull = memo!(1,0)
    trigs = memo!(-1, -2) + memo!(-1, 2) + memo!(1, -2) + memo!(1, 2)
    pentas = memo!(-1, -1) + memo!(-1, 1) + memo!(1, -1) + memo!(1, 1)
    spikes = memo!(-2, 0) + memo!(0, 2) + memo!(2, 0) + memo!(0, -2)

    memo = accumArray (+) 0 ((-2, -2), (2, 2))
      [((y `div` 131, x `div` 131), 1) | (y, x) <- fields]

-- Visualization of part two input data was very important for solving this
-- problem. This puzzle input is extremely special.
visualize :: Int -> Garden -> Pos -> [Char]
visualize steps garden start =
  intercalate "\n\n"
  [ intercalate "\n"
    [ intercalate "   "
      [ [getField (y, x) | x <- [sx*size..(sx+1)*size-1]]
      | sx <- [-scale..scale]
      ]
    | y <- [sy*size..(sy+1)*size-1]
    ]
  | sy <- [-scale..scale]
  ]
  where
    scale = 5
    ((a, _), (b, _)) = bounds garden
    size = b - a + 1
    visited = Set.fromList $ reachablePositions steps garden start
    getField p
      | p `Set.member` visited = 'O'
      | otherwise = garden!(wrapAround garden p)

main :: IO ()
main = do
  (garden, start) <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne garden start)
  putStrLn $ "Part Two: " ++ show (partTwo garden)
