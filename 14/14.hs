import Data.List (transpose)

type Platform = [[Char]]

rotateCW90 :: Platform -> Platform
rotateCW90 = transpose . reverse

slideRight :: Platform -> Platform
slideRight = map slide
  where
    slide = uncurry (++) . foldr f ([], [])
    f '.' (dots, acc) = ('.':dots, acc)
    f 'O' (dots, acc) = (dots, 'O':acc)
    f '#' (dots, acc) = ([], '#':(dots++acc))

loadOnRightBeams :: Platform -> Int
loadOnRightBeams = sum . map load
  where
    load = sum . zipWith (\w c -> if c == 'O' then w else 0) [1..]

partOne :: Platform -> Int
partOne = loadOnRightBeams . slideRight . rotateCW90

spin :: Platform -> Platform
spin plat = iterate (slideRight . rotateCW90) plat !! 4

-- Infinite list of platform spins. It uses the "Tying the Knot" technique
-- to limit computations and memory usage. That is, the list is actually
-- cyclic.
spins :: Platform -> [Platform]
spins = go []
  where
    go acc p
      | p `elem` acc = let r = (reverse acc) ++ (dropWhile (/= p) r) in r
      | otherwise    = go (p : acc) (spin p)

-- partTwo :: Platform -> Int
partTwo :: Platform -> Int
partTwo = loadOnRightBeams . rotateCW90 . (!! 1000000000) . spins

main :: IO ()
main = do
  platform <- lines <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne platform)
  putStrLn $ "Part Two: " ++ show (partTwo platform)
