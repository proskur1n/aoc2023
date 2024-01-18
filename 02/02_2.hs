import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isLetter)
import Data.Maybe (fromMaybe)

type Handful = (Int, Int, Int)
type Game = (Int, [Handful])

cubes :: ReadP (String, Int)
cubes = do
    n <- read <$> munch1 isDigit
    char ' '
    color <- munch1 isLetter
    return (color, n)

handful :: ReadP Handful
handful = do
    colors <- sepBy1 cubes (string ", ")
    let lookupColor = fromMaybe 0 . flip lookup colors
    return (lookupColor "red", lookupColor "green", lookupColor "blue")

game :: ReadP Game
game = do
    string "Game "
    i <- read <$> munch1 isDigit
    string ": "
    hands <- sepBy1 handful (string "; ")
    eof
    return (i, hands)

parseGame :: String -> Game
parseGame line = let [(g, "")] = readP_to_S game line in g

power :: Game -> Int
power (_, hands) = maxR * maxG * maxB
  where
    maxR = maximum $ map (\(r, _, _) -> r) hands
    maxG = maximum $ map (\(_, g, _) -> g) hands
    maxB = maximum $ map (\(_, _, b) -> b) hands

interact' :: Show s => (String -> s) -> IO ()
interact' f = interact (\s -> show (f s) ++ "\n")

main :: IO ()
main = interact' (sum . map (power . parseGame) . lines)
