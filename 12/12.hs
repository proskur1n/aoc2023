{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad.State.Strict (State, gets, modify, evalState)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (uncons, intercalate)

type Row = (String, [Int])
type Memo = Map.Map Row Int

memoized :: (String -> [Int] -> State Memo Int) -> String -> [Int] -> State Memo Int
memoized fn s ns = do
    memo <- gets (Map.lookup (s, ns))
    case memo of
        Just v ->
            return v
        Nothing -> do
            v <- fn s ns
            modify (Map.insert (s, ns) v)
            return v

arrangements :: String -> [Int] -> State Memo Int
arrangements s ns =
    case dropWhile (== '.') s of
        '?':rest -> do
            x <- memoized springGroup ('#':rest) ns
            y <- memoized arrangements rest ns
            return (x + y)
        '#':rest ->
            memoized springGroup ('#':rest) ns
        "" ->
            return if null ns then 1 else 0

springGroup :: String -> [Int] -> State Memo Int
springGroup _ [] = return 0
springGroup s (n:ns)
    | prefix == replicate n '#' && c /= '#' = memoized arrangements rest ns
    | otherwise = return 0
    where
        prefix = map (\case '?' -> '#'; c -> c) $ take n s
        (c, rest) = fromMaybe ('.', "") $ uncons $ drop n s

parse :: String -> [Row]
parse = map parseLine . lines
    where
        parseLine s = let [springs, groups] = words s in (springs, split groups)
        split "" = []
        split s = let (num, rest) = break (== ',') s in read num : split (drop 1 rest)

partOne :: [Row] -> Int
partOne = sum . map (\(s, ns) -> evalState (arrangements s ns) Map.empty)

partTwo :: [Row] -> Int
partTwo = partOne . map unfold
    where
        unfold (s, ns) = (intercalate "?" (replicate 5 s), concat (replicate 5 ns))

main :: IO ()
main = readFile "input" >>= (print . partTwo . parse)
