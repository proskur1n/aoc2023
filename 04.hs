parseLine :: String -> ([Int], [Int])
parseLine line =
    let
        (winning, _:drawn) = span (/= "|") $ drop 2 $ words line
    in
        (map read winning, map read drawn)

matchingNumbers :: String -> Int
matchingNumbers line =
    let
        (winning, drawn) = parseLine line
    in
        length $ filter (`elem` winning) drawn

gamePoints :: String -> Int
gamePoints line =
    let
        matching = matchingNumbers line
    in
        if matching == 0 then 0 else 2 ^ (matching - 1)

partOne :: String -> Int
partOne = sum . map gamePoints . lines

contributions :: [Int] -> [Int]
contributions [_]    = [1]
contributions (x:xs) = (1 + sum (take x $ contributions xs)) : contributions xs

partTwo :: String -> Int
partTwo = sum . contributions . map matchingNumbers . lines

interact' :: Show s => (String -> s) -> IO ()
interact' f = interact (\s -> show (f s) ++ "\n")

main :: IO ()
main = interact' partTwo
