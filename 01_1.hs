import Data.Char (isDigit, digitToInt)
import Data.List (find)
import Data.Maybe (fromJust)

calibration :: String -> Int
calibration line = 10 * findDigit line + findDigit (reverse line)
  where
    findDigit :: String -> Int
    findDigit = digitToInt . fromJust . find isDigit

interact' :: Show s => (String -> s) -> IO ()
interact' f = interact (\s -> show (f s) ++ "\n")

main :: IO ()
main = interact' (sum . map calibration . lines)
