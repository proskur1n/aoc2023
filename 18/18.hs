{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Read (decimal, hexadecimal)
import Data.List (scanl')

type Pos = (Int, Int)
type Dist = Int
data Dir = U | D | L | R deriving (Show, Read, Eq, Enum)
data Curvature = Convex | Concave deriving (Show, Read, Eq, Enum)
type CubicMeters = Int

outline :: [(Dir, Dist)] -> [Pos]
outline instructions =
  let
    (dirs, dists) = unzip instructions
    curvatures = zipWith curv (last dirs : dirs) dirs
    dists' = zipWith3 resize dists curvatures (tail curvatures)
  in
    scanl' move (0, 0) $ zip dirs dists'
  where
    curv U R = Convex
    curv R D = Convex
    curv D L = Convex
    curv L U = Convex
    curv _ _ = Concave

    resize dist Convex Convex = dist + 1
    resize dist Concave Concave = dist - 1
    resize dist _ _ = dist

    move (x, y) (U, dist) = (x, y + dist)
    move (x, y) (D, dist) = (x, y - dist)
    move (x, y) (L, dist) = (x - dist, y)
    move (x, y) (R, dist) = (x + dist, y)

-- https://en.wikipedia.org/wiki/Shoelace_formula
shoelace :: [Pos] -> CubicMeters
shoelace (unzip -> (xs, ys)) =
  let
    a = sum $ zipWith (*) xs (tail ys ++ [head ys])
    b = sum $ zipWith (*) ys (tail xs ++ [head xs])
  in
    abs (a - b) `div` 2

partOne :: Text -> CubicMeters
partOne = shoelace . outline . map parseLine . Text.lines
  where
    parseLine ln =
      let
        [dir, dist, _] = Text.words ln
      in
        (readDir dir, either error fst $ decimal dist)

    readDir = \case
      "U" -> U
      "D" -> D
      "L" -> L
      "R" -> R

partTwo :: Text -> CubicMeters
partTwo = shoelace . outline . map parseLine . Text.lines
  where
    parseLine ln =
      let
        [_, _, color] = Text.words ln
        (dist, dir) = Text.splitAt 5 $ Text.take 6 $ Text.drop 2 color
      in
        (readDir dir, readHex dist)

    readHex = fst . either error id . hexadecimal

    readDir = \case
      "0" -> R
      "1" -> D
      "2" -> L
      "3" -> U

main :: IO ()
main = do
  input <- TextIO.readFile "input"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
