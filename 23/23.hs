import Data.Array (Array)
import qualified Data.Array as Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

type Pos = (Int, Int) -- (y, x)
type Plan = Array Pos Char
type Start = Pos
type Finish = Pos
type Graph = Map Pos [(Pos, Int)]

parse :: String -> Plan
parse input =
  let
    lines' = lines input
    rows = length lines'
    cols = length $ head lines'
  in
    Array.listArray ((1, 1), (rows, cols)) $ concat lines'

-- Finding longest path in a unidirected graph is NP-Hard. We simplify the
-- given input grid to a much smaller graph of important points (junctions,
-- start and finish points). The simplified graph can be brute-forced in
-- reasonable time.
constructGraph :: (Pos -> [Pos]) -> Plan -> (Graph, Start, Finish)
constructGraph getNeighbors plan = (graph, start, finish)
  where
    ((y0, x0), (y1, x1)) = Array.bounds plan
    start = (y0, x0 + 1)
    finish = (y1, x1 - 1)
    (graph, _) = go start 0 start (Map.empty, Set.empty) start

    go lastCrossing distance prevPos (graph, visited) pos
      | important =
        let
          graph' = insertEdge graph (lastCrossing, pos) distance
        in
          if pos == finish || pos `Set.member` visited then
            (graph', visited)
          else
            foldl' (go pos 1 pos) (graph', visited') neighbors
      | otherwise =
        foldl' (go lastCrossing (distance + 1) pos) (graph, visited') $ filter (/= prevPos) neighbors
      where
        neighbors = filter walkable $ getNeighbors pos
        important = pos == finish || length neighbors > 2
        visited' = Set.insert pos visited

    walkable pos =
      Array.inRange (Array.bounds plan) pos && (plan Array.! pos) /= '#'

    insertEdge graph (p0, p1) weight =
      Map.insertWith (++) p0 [(p1, weight)] graph

exhaustiveSearch :: Graph -> Start -> Finish -> Int
exhaustiveSearch graph start finish = go 0 minBound Set.empty start
  where
    go distance longest visited pos
      | pos `Set.member` visited = longest
      | pos == finish = max distance longest
      | otherwise =
        let
          adjacent = graph Map.! pos
          visited' = Set.insert pos visited
        in
          maximum $ map (\(adj, weight) -> go (distance + weight) longest visited' adj) adjacent

partOne :: Plan -> Int
partOne plan =
  let
    (graph, start, finish) = constructGraph getNeighbors plan
  in
    exhaustiveSearch graph start finish
  where
    getNeighbors (y, x) =
      case plan Array.! (y, x) of
        '.' -> [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
        '^' -> [(y - 1, x)]
        '>' -> [(y, x + 1)]
        'v' -> [(y + 1, x)]
        '<' -> [(y, x - 1)]

partTwo :: Plan -> Int
partTwo plan =
  let
    (graph, start, finish) = constructGraph getNeighbors plan
  in
    exhaustiveSearch graph start finish
  where
    getNeighbors (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

main :: IO ()
main = do
  plan <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne plan)
  putStrLn $ "Part Two: " ++ show (partTwo plan)
