import Data.List (transpose)

type Sketch = [[Char]]

vertical :: Int -> Sketch -> Int
vertical smudges (x:xs) = go [x] xs
  where
    go _ [] = 0
    go top bottom
      | smudges == differences = length top
      | otherwise              = go (head bottom : top) (tail bottom)
      where
        differences = length $ filter (uncurry (/=)) $ zip (concat top) (concat bottom)

parse :: String -> [Sketch]
parse = paragraphs . lines
  where
    paragraphs [] = []
    paragraphs ls =
      let
        (p, rest) = break null ls
      in
        p : paragraphs (drop 1 rest)

solve :: Int -> [Sketch] -> Int
solve smudges sketches = 100 * v + h
  where
    v = sum $ map (vertical smudges) sketches
    h = sum $ map (vertical smudges . transpose) sketches

partOne :: [Sketch] -> Int
partOne = solve 0

partTwo :: [Sketch] -> Int
partTwo = solve 1

main :: IO ()
main = do
  sketches <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne sketches)
  putStrLn $ "Part Two: " ++ show (partTwo sketches)
