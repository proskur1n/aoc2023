import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.List (isSuffixOf, scanl')

data Turn = L | R deriving (Show, Eq)
type Node = String
type Fork = (Node, Node)
data Document = Document [Turn] (Map Node Fork) deriving Show

parse :: String -> Document
parse input =
    case readP_to_S document input of
        [(doc, "")] -> doc
        v -> error $ "failed to parse input: " ++ show v
  where
    document :: ReadP Document
    document = do
        turns <- many1 ((L <$ char 'L') <|> (R <$ char 'R'))
        skipSpaces
        forks <- endBy1 fork skipSpaces
        eof
        return $ Document turns (Map.fromList forks)

    fork :: ReadP (Node, Fork)
    fork = do
        n <- node
        string " = ("
        l <- node
        string ", "
        r <- node
        char ')'
        return (n, (l, r))

    node :: ReadP Node
    node = count 3 (satisfy isAlphaNum)

travel :: (Node -> Bool) -> Map Node Fork -> [Turn] -> Node -> Int
travel finished network turns start =
    length $ takeWhile (not . finished) $ scanl' decidePath start (cycle turns)
  where
    decidePath :: Node -> Turn -> Node
    decidePath n L = fst (network Map.! n)
    decidePath n R = snd (network Map.! n)

partOne :: Document -> Int
partOne (Document turns forks) =
    travel (== "ZZZ") forks turns "AAA"

partTwo :: Document -> Int
partTwo (Document turns forks) =
  let
    start = filter ("A" `isSuffixOf`) (Map.keys forks)
  in
    -- We assume that there is a only a single finish node for each starting
    -- point and that all paths are cyclic. Otherwise, this trick with `lcm`
    -- would not work. There is nothing in the task description which says it
    -- has to be case but it works for this input data.
    foldl1 lcm $ map (travel ("Z" `isSuffixOf`) forks turns) start

main :: IO ()
main = readFile "input" >>= (print . partTwo . parse)
