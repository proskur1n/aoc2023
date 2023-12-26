import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Read (decimal)
import Data.Ix (range)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl', mapAccumL, nub, sortOn)
import Data.Maybe (mapMaybe)

type Pos = (Int, Int, Int)
type Brick = (Pos, Pos)

parse :: Text -> [Brick]
parse = map brick . Text.lines
  where
    brick s =
      let
        [x0, y0, z0, x1, y1, z1] = map readInt $ Text.split (`elem` ",~") s
      in
        ((x0, y0, z0), (x1, y1, z1))

    readInt s =
      case decimal s of
        Left err -> error err
        Right (val, _) -> val

-- Returns a map from bricks to a list of bricks that brick stands on. If all
-- bricks in this list would disappear the first brick would fall.
buildSupportGraph :: [Brick] -> Map Brick [Brick]
buildSupportGraph bricks =
  let
    sorted = sortOn brickZ bricks
    (_, supportGraph) = foldl' dropBrick (Map.empty, Map.empty) sorted
  in
    supportGraph

  where
    brickZ ((_, _, z), _) = z

    dropBrick (ground, supportGraph) brick@((x0, y0, z0), (x1, y1, z1)) =
      let
        affected = range ((x0, y0), (x1, y1))

        bricksBelow = mapMaybe (`Map.lookup` ground) affected
        restingZ =
          case bricksBelow of
            [] -> 0
            xs -> maximum $ map fst xs
        support = nub [b | (z, b) <- bricksBelow, z == restingZ]

        newZ = restingZ + z1 - z0 + 1
        ground' = foldl' (\m p -> Map.insert p (newZ, brick) m) ground affected
      in
        (ground', Map.insert brick support supportGraph)

invert :: Ord v => Map k [v] -> Map v [k]
invert m = Map.fromListWith (++) [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]

partOne :: Map Brick [Brick] -> Int
partOne graph = Map.size graph - Map.size (invert $ Map.filter (\xs -> length xs == 1) graph)

partTwo :: Map Brick [Brick] -> Int
partTwo supportGraph = sum $ map disintegrate $ Map.keys supportGraph
  where
    invertedSupportGraph = invert supportGraph

    disintegrate brick =
      let
        (_, fallen) = disintegrateBricksAbove Set.empty brick
      in
        sum fallen

    disintegrateBricksAbove removed brick =
      let
        bricksAbove = Map.findWithDefault [] brick invertedSupportGraph
      in
        mapAccumL tryDropBrick (Set.insert brick removed) bricksAbove

    tryDropBrick removed brick =
      let
        bricksBelow = supportGraph ! brick
      in
        if all (`Set.member` removed) bricksBelow then
          let
            (removed', fallen) = disintegrateBricksAbove removed brick
          in
            (removed', 1 + sum fallen)
        else
          (removed, 0)

main :: IO ()
main = do
  supportGraph <- buildSupportGraph . parse <$> TextIO.readFile "input"
  putStrLn $ "Part One: " ++ show (partOne supportGraph)
  putStrLn $ "Part Two: " ++ show (partTwo supportGraph)
