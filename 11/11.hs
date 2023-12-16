{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE LambdaCase #-}
import Data.List (transpose, tails, mapAccumL)
import Data.Maybe (mapMaybe)
import Flow ((|>))

type Position = (Int, Int)

data Observation = EmptySpace | Galaxy Position deriving (Show, Eq)

parse :: String -> [[Observation]]
parse input =
    [ [if c == '#' then Galaxy (x, y) else EmptySpace | c <- row | x <- [1 ..]]
      | row <- lines input
      | y <- [1 ..]
    ]

sumWithoutExpansion :: [[Observation]] -> Int
sumWithoutExpansion galaxies =
    concat galaxies
    |> mapMaybe (\case Galaxy p -> Just p; _ -> Nothing)
    |> pairs
    |> map (uncurry manhattan)
    |> sum
    where
        pairs :: [a] -> [(a, a)]
        pairs list = [(x, y) | (x:ys) <- tails list, y <- ys]

        manhattan :: Position -> Position -> Int
        manhattan (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

expandUniverse :: Int -> [[Observation]] -> [[Observation]]
expandUniverse factor = expandRows . transpose' . expandRows
    where
        transpose' :: [[Observation]] -> [[Observation]]
        transpose' = transpose . map (map (\case Galaxy (x, y) -> Galaxy (y, x); _ -> EmptySpace))

        expandRows :: [[Observation]] -> [[Observation]]
        expandRows = snd . mapAccumL expandRow 0

        expandRow :: Int -> [Observation] -> (Int, [Observation])
        expandRow e row =
            if all (== EmptySpace) row then
                (e + factor - 1, row)
            else
                (e, map (\case Galaxy (x, y) -> Galaxy (x, y + e); _ -> EmptySpace) row)

partOne :: [[Observation]] -> Int
partOne = sumWithoutExpansion . expandUniverse 2

partTwo :: [[Observation]] -> Int
partTwo = sumWithoutExpansion . expandUniverse 1000000

main :: IO ()
main = readFile "input" >>= (print . partTwo . parse)
