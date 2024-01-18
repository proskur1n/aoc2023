{-# LANGUAGE ParallelListComp #-}

import Data.Char (isDigit)
import Data.List (groupBy)

data Region = Region
    { fromX :: Int
    , toX   :: Int
    , y     :: Int
    }

adjacent :: Region -> (Int, Int) -> Bool
adjacent (Region x0 x1 y) (x', y') =
    (x0 - 1) <= x' && x' <= (x1 + 1) && (y - 1) <= y' && y' <= (y + 1)

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy fn list
    | null stripped = []
    | (word, rest) <- span fn stripped = word : wordsBy fn rest
  where
    stripped = dropWhile (not . fn) list

toIndexed :: [String] -> [(Int, Int, Char)]
toIndexed lines@(first:_) =
    [(x, y, c) | y <- [0..(length lines-1)],
                 x <- [0..(length first-1)]
               | c <- concat lines]

symbols :: (Char -> Bool) -> [(Int, Int, Char)] -> [(Int, Int)]
symbols fn = map (\(x, y, _) -> (x, y)) . filter (\(_, _, c) -> fn c)

numbers :: [(Int, Int, Char)] -> [(Int, Region)]
numbers = concatMap lineToNumbers . splitLines
  where
    splitLines :: [(Int, Int, Char)] -> [[(Int, Int, Char)]]
    splitLines = groupBy (\(_, y, _) (_, y', _) -> y == y')

    lineToNumbers :: [(Int, Int, Char)] -> [(Int, Region)]
    lineToNumbers = map wordToNumber . wordsBy (\(_, _, c) -> isDigit c)

    wordToNumber :: [(Int, Int, Char)] -> (Int, Region)
    wordToNumber chars =
        let (x,  y, _) = head chars
            (x', _, _) = last chars
        in (read $ map (\(_, _, c) -> c) chars, Region x x' y)

getPartNumbers :: [String] -> [Int]
getPartNumbers lines =
    let symbols' = symbols (\c -> not (c == '.' || isDigit c)) (toIndexed lines)
        numbers' = numbers (toIndexed lines)
    in [n | (n, region) <- numbers', any (adjacent region) symbols']

interact' :: Show s => (String -> s) -> IO ()
interact' f = interact (\s -> show (f s) ++ "\n")

main :: IO ()
main = interact' (sum . getPartNumbers . lines)
