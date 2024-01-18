import Data.Char (isDigit, digitToInt)
import Data.List (tails, isPrefixOf)
import Data.Maybe (mapMaybe)

extractNumber :: String -> Maybe Int
extractNumber str
  | (c:_) <- str, isDigit c  = Just $ digitToInt c
  | "one"   `isPrefixOf` str = Just 1
  | "two"   `isPrefixOf` str = Just 2
  | "three" `isPrefixOf` str = Just 3
  | "four"  `isPrefixOf` str = Just 4
  | "five"  `isPrefixOf` str = Just 5
  | "six"   `isPrefixOf` str = Just 6
  | "seven" `isPrefixOf` str = Just 7
  | "eight" `isPrefixOf` str = Just 8
  | "nine"  `isPrefixOf` str = Just 9
  | otherwise                = Nothing

calibration :: String -> Int
calibration line = 10 * head numbers + last numbers
  where numbers = mapMaybe extractNumber (tails line)

interact' :: Show s => (String -> s) -> IO ()
interact' f = interact (\s -> show (f s) ++ "\n")

main :: IO ()
main = interact' (sum . map calibration . lines)
