-- Assumes that all rows are reversed.
nextRow :: [Int] -> [Int]
nextRow (a:b:rest) = (a - b) : nextRow (b : rest)
nextRow _ = []

predict :: [Int] -> Int
predict nums =
    sum $ map head $ take (length nums) $ iterate nextRow $ reverse nums

partOne :: [[Int]] -> Int
partOne = sum . map predict

partTwo :: [[Int]] -> Int
partTwo = partOne . map reverse

main :: IO ()
main = readFile "input" >>= (print . partTwo . map (map read . words) . lines)
