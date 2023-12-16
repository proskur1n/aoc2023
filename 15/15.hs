{-# LANGUAGE ParallelListComp #-}
import Data.Char (isSpace, ord)
import Data.Word (Word8)
import Data.List (foldl')
import Data.Array (Array, Ix, accumArray, assocs)

type Hash = Word8

hash :: String -> Hash
hash = foldl' (\acc c -> (acc + fromIntegral (ord c)) * 17) 0

split :: Char -> String -> [String]
split _ "" = []
split delim s =
  let (a, b) = break (== delim) s
  in a : split delim (drop 1 b)

parse :: String -> [String]
parse = split ',' . filter (not . isSpace)

partOne :: [String] -> Int
partOne = sum . map (fromIntegral . hash)

type Label = String
type FocalLength = Int
data Lens = Lens { label :: Label, focalLength :: FocalLength }
data Op = Add Lens | Remove Label
type Box = [Lens]

partTwo :: [String] -> Int
partTwo = sum . map focusingPower . assocs . accumArray apply [] (minBound, maxBound) . map op
  where
    add :: Box -> Lens -> Box
    add [] lens = [lens]
    add (x:xs) lens
      | label lens == label x = lens : xs
      | otherwise = x : add xs lens

    remove :: Box -> Label -> Box
    remove [] _ = []
    remove (x:xs) lbl
      | lbl == label x = xs
      | otherwise = x : remove xs lbl

    op :: String -> (Hash, Op)
    op s =
      case split '=' s of
        [lbl, focal] -> (hash lbl, Add $ Lens lbl (read focal))
        [lbl] -> (hash $ init lbl, Remove $ init lbl)

    apply :: Box -> Op -> Box
    apply box (Add lens) = add box lens
    apply box (Remove lbl) = remove box lbl

    focusingPower :: (Hash, Box) -> Int
    focusingPower (h, box) = sum [(fromIntegral h + 1) * i * focal | (Lens lbl focal) <- box | i <- [1..]]

main :: IO ()
main = do
  steps <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne steps)
  putStrLn $ "Part Two: " ++ show (partTwo steps)
