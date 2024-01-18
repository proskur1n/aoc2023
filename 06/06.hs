import Data.Char (isDigit)

type Type     = Int
type Distance = Int

waysToWin :: (Type, Distance) -> Int
waysToWin (t, d) =
    let
        s = sqrt (fromIntegral $ t^2 - 4 * d)
        x1 = (fromIntegral t - s) / 2
        x2 = (fromIntegral t + s) / 2
    in
        ceiling x2 - floor x1 - 1

partOne :: String -> Int
partOne input = product $ map waysToWin races
  where
    ((_:times):(_:distances):_) = map words $ lines input
    races = zipWith (\a b -> (read a, read b)) times distances

partTwo :: String -> Int
partTwo input = waysToWin (time, dist)
  where
    (first:second:_) = lines input
    time = read $ filter isDigit first
    dist = read $ filter isDigit second

main :: IO ()
main = readFile "input" >>= (print . partTwo)
