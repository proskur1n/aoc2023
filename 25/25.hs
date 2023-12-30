import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List (delete, foldl')
import Control.Monad.State.Strict (State, state, evalState)
import System.Random (RandomGen, uniformR, mkStdGen)

type Component = String
type Graph = Map Component [Component]

parse :: String -> Graph
parse = foldl' addComponent Map.empty . map parseLine . lines
  where
    parseLine line =
      let (name:connections) = words line
      in (init name, connections)

    addComponent graph (name, connections) =
      let backwardEdges = map (, [name]) connections
      in Map.unionWith (++) (Map.fromList $ (name, connections):backwardEdges) graph

-- https://en.wikipedia.org/wiki/Karger%27s_algorithm
karger :: RandomGen g => Graph -> State g (Int, Int, Int)
karger graph = go graph (1 <$ graph)
  where
    go graph contracted
      | Map.size graph == 2 =
        let
          [(u, edges), (v, _)] = Map.toList graph
        in
          return (length edges, contracted ! u, contracted ! v)
      | otherwise = do
        (u, v) <- randomEdge graph
        let graph' = contract graph u v
        let contracted' = Map.adjust (+ contracted ! v) u contracted
        go graph' contracted'

    contract graph u v =
      let
        vAdj = filter (/= u) (graph ! v)
        newEdges = concatMap (\n -> [(u, [n]), (n, [u])]) vAdj
      in
        Map.unionWith (++) (Map.fromListWith (++) newEdges) (removeNode graph v)

    removeNode graph v =
      let
        vAdj = (graph Map.! v)
      in
        foldr (Map.adjust (delete v)) (Map.delete v graph) vAdj

    randomEdge graph = do
      i <- state $ uniformR (0, Map.size graph - 1)
      let (u, vs) = Map.elemAt i graph
      j <- state $ uniformR (0, length vs - 1)
      return (u, vs !! j)

partOne :: Graph -> Int
partOne graph = evalState (go graph) (mkStdGen 42)
  where
    go graph = do
      (minCut, group0, group1) <- karger graph
      if minCut == 3 then
        return (group0 * group1)
      else
        go graph

main :: IO ()
main = do
  graph <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne graph)
