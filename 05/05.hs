{-# LANGUAGE OverloadedRecordDot #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

data Interval = Interval
    { begin :: Int -- inclusive
    , end   :: Int -- inclusive
    , delta :: Int -- values in interval [begin, end] are mapped to [begin+delta, end+delta]
    } deriving Show
type Mapping  = [Interval]
type Seed     = Int
type Location = Int
type Almanac  = ([Seed], [Mapping])

parse :: String -> Almanac
parse input =
    case readP_to_S almanac input of
        [(a, "")] -> a
        v         -> error ("failed to parse input: " ++ show v)
  where
    almanac :: ReadP Almanac
    almanac = do
        seeds <- section number
        sections <- many1 (section triple)
        eof
        return (seeds, sections)

    section :: ReadP p -> ReadP [p]
    section p = do
        skipMany1 $ satisfy (not . isDigit)
        endBy1 p skipSpaces

    triple :: ReadP Interval
    triple = do
        dest <- number
        src <- skipSpaces >> number
        len <- skipSpaces >> number
        return $ Interval src (src + len - 1) (dest - src)

    number :: ReadP Int
    number = read <$> munch1 isDigit

forward :: [Mapping] -> Seed -> Location
forward maps seed = foldl forw seed maps
  where
    forw s [] = s
    forw s (x:xs)
        | x.begin <= s && s <= x.end = s + x.delta
        | otherwise = forw s xs

backward :: [Mapping] -> Location -> Seed
backward maps loc = foldr back loc maps
  where
    back [] loc = loc
    back (x:xs) loc
        | x.begin <= loc' && loc' <= x.end = loc'
        | otherwise = back xs loc
        where loc' = loc - x.delta

partOne :: Almanac -> Location
partOne (seeds, maps) = minimum $ map (forward maps) seeds

partTwo :: Almanac -> Location
partTwo (seeds, maps) = head $ filter (isValidSeed seeds . backward maps) [0..]
  where
    isValidSeed [] _ = False
    isValidSeed (beg:len:rest) s = beg <= s && s < beg + len || isValidSeed rest s

main :: IO ()
main = readFile "input" >>= (print . partTwo . parse)
