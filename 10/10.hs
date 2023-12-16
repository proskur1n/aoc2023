{-# LANGUAGE LambdaCase #-}
import Data.Array

data Tile
    = Ground
    | NorthSouth
    | EastWest
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    deriving (Show, Eq)

type Position = (Int, Int) -- (y, x)

parse :: String -> (Position, Array Position Tile)
parse input =
    let
        rows = lines input
        w = length $ head rows
        h = length rows
        grid = listArray ((1, 1), (h, w)) $ concat rows

        s = SouthWest -- Hardcoded for this particular input.
        sPos = fst $ head $ filter (\(_, c) -> c == 'S') $ assocs grid

        toTile :: Char -> Tile
        toTile = \case
            '|' -> NorthSouth
            '-' -> EastWest
            'L' -> NorthEast
            'J' -> NorthWest
            '7' -> SouthWest
            'F' -> SouthEast
            _ -> Ground
    in
        (sPos, (toTile <$> grid) // [(sPos, s)])

pathInsidePipe :: Array Position Tile -> Position -> [Position]
pathInsidePipe grid start =
    let
        p0 = start
        p1 = fst (fork p0)
        path = p0 : p1 : zipWith move path (tail path)

        move :: Position -> Position -> Position
        move prevPos pos =
            let
                (a, b) = fork pos
            in
                if a == prevPos then b else a

        fork :: Position -> (Position, Position)
        fork (y, x) =
            case grid!(y, x) of
                NorthSouth -> ((y - 1, x), (y + 1, x))
                EastWest -> ((y, x - 1), (y, x + 1))
                NorthEast -> ((y - 1, x), (y, x + 1))
                NorthWest -> ((y - 1, x), (y, x - 1))
                SouthWest -> ((y + 1, x), (y, x - 1))
                SouthEast -> ((y + 1, x), (y, x + 1))
    in
        start : takeWhile (/= start) (tail path)

partOne :: Position -> Array Position Tile -> Int
partOne start grid = length (pathInsidePipe grid start) `div` 2

partTwo :: Position -> Array (Int, Int) Tile -> Int
partTwo start grid =
    let
        -- Replace all tiles that are not part of the main loop with the `Ground` tile.
        onlyLoop = listArray (bounds grid) (repeat Ground) // map (\p -> (p, grid!p)) (pathInsidePipe grid start)
        ((y0, x0), (y1, x1)) = bounds grid

        oddEvenRule :: Int -> Int -> Bool -> Int
        oddEvenRule x y inside
            | x > x1
                = 0
            | onlyLoop!(y,x) == Ground
                = fromEnum inside + oddEvenRule (x + 1) y inside
            | onlyLoop!(y, x) `elem` [NorthSouth, SouthEast, SouthWest]
                = oddEvenRule (x + 1) y (not inside)
            | otherwise
                = oddEvenRule (x + 1) y inside
    in
        sum [oddEvenRule x0 y False | y <- [y0..y1]]

main :: IO ()
main = readFile "input" >>= (print . uncurry partTwo . parse)
