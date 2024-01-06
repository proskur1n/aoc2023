{-# LANGUAGE OverloadedRecordDot #-}
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Linear
import Control.Lens ((^.))

data Ray = Ray
  { pos :: V3 Rational
  , dir :: V3 Rational
  } deriving (Show, Eq)

parse :: String -> [Ray]
parse = map toHail . lines
  where
    toHail :: String -> Ray
    toHail s =
      let
        [x, y, z, vx, vy, vz] = map (fromIntegral . read) (split (\c -> not (isDigit c || c == '-')) s)
      in
        Ray (V3 x y z) (V3 vx vy vz)

    split :: (Char -> Bool) -> String -> [String]
    split isWhitespace str =
      case dropWhile isWhitespace str of
        [] -> []
        s  -> let (word, rest) = break isWhitespace s
              in word : split isWhitespace rest

partOne :: [Ray] -> Int
partOne = length . filter withinArea . mapMaybe (uncurry intersectXY) . pairs
  where
    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs (x:xs) = map (x, ) xs ++ pairs xs

    intersectXY :: Ray -> Ray -> Maybe (V2 Rational)
    intersectXY r0 r1 =
      let
        det = -(r0.dir^._x * r1.dir^._y - r0.dir^._y * r1.dir^._x)
        (V3 dx dy _) = r1.pos - r0.pos
        s = (r1.dir^._x * dy - r1.dir^._y * dx) / det
        t = (r0.dir^._x * dy - r0.dir^._y * dx) / det
      in
        if det == 0 || s < 0 || t < 0 then
          Nothing
        else
          Just $ (r0.pos ^. _xy) + s *^ (r0.dir ^. _xy)

    lower :: Rational
    lower = 200000000000000

    upper :: Rational
    upper = 400000000000000

    withinArea :: V2 Rational -> Bool
    withinArea (V2 x y) = lower <= x && x <= upper && lower <= y && y <= upper

-- Inspired by this brilliant solution:
-- https://github.com/MarkSinke/aoc2023/blob/541253f11cb768ae3d9c8e9c996673431142ad33/day24.go#L292
partTwo :: [Ray] -> Int
partTwo (h0:h1:h2:h3:_) =
  let
    -- Move point of reference to h0.
    r1 = Ray (h1.pos - h0.pos) (h1.dir - h0.dir)
    r2 = Ray (h2.pos - h0.pos) (h2.dir - h0.dir)
    r3 = Ray (h3.pos - h0.pos) (h3.dir - h0.dir)

    -- Span plane going through (0, 0, 0) and aligned with ray r1.
    normal = r1.pos `cross` (r1.pos + r1.dir)

    (timeA, pointA) = rayPlaneIntersection r2 (pure 0) normal
    (timeB, pointB) = rayPlaneIntersection r3 (pure 0) normal

    rdir = (pointB - pointA) ^/ (timeB - timeA)
    rpos = pointA - timeA *^ rdir

    -- Tranform rock position back to the original coordinate system.
    (V3 rockX rockY rockZ) = round <$> rpos + h0.pos
  in
    rockX + rockY + rockZ
  where
    rayPlaneIntersection :: Ray -> V3 Rational -> V3 Rational -> (Rational, V3 Rational)
    rayPlaneIntersection r point normal =
      let
        t = normal `dot` (point - r.pos) / normal `dot` r.dir
      in
        (t, r.pos + t *^ r.dir)

main :: IO ()
main = do
  hailstones <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne hailstones)
  putStrLn $ "Part Two: " ++ show (partTwo hailstones)
