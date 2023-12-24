{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Sequence (Seq, Seq((:<|)), (><))
import qualified Data.Sequence as Seq
import Control.Monad.Trans.State (State, modify, runState)
import Data.List (partition)

data Pulse = Low | High deriving (Show, Eq)

type Module = Text

data ModuleState = FlipFlop Bool | Conjunction (Map Module Pulse) | Broadcaster

data Signal = Signal Module Pulse Module deriving (Show, Eq)

invert :: Ord v => Map k [v] -> Map v [k]
invert m = Map.fromListWith (++) [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]

parse :: Text -> (Map Module [Module], Map Module ModuleState)
parse input = (network, Map.fromList $ zip names states)
  where
    (names, connections, states) = unzip3 $ map parseLine $ Text.lines input
    network = Map.fromList $ zip names connections
    network' = invert network

    parseLine :: Text -> (Module, [Module], ModuleState)
    parseLine line
      | Just ('%', name') <- Text.uncons name = (name', dest, FlipFlop False)
      | Just ('&', name') <- Text.uncons name = (name', dest, conjunction name')
      | otherwise = (name, dest, Broadcaster)
      where
        (name:"->":dest) = Text.words $ Text.filter (/= ',') line

    conjunction :: Module -> ModuleState
    conjunction m = Conjunction $ Map.fromList $ map (, Low) (network' ! m)

process :: ModuleState -> Signal -> (ModuleState, Maybe Pulse)
process (FlipFlop on) (Signal _ High _) = (FlipFlop on, Nothing)
process (FlipFlop False) (Signal _ Low _) = (FlipFlop True, Just High)
process (FlipFlop True) (Signal _ Low _) = (FlipFlop False, Just Low)
process (Conjunction pulses) (Signal src pulse _) =
  let
    pulses' = Map.insert src pulse pulses
  in
    (Conjunction pulses', if all (== High) pulses' then Just Low else Just High)
process Broadcaster (Signal _ pulse _) = (Broadcaster, Just pulse)

pushButton
  :: Map Module [Module]
  -> Map Module ModuleState
  -> State [Signal] (Map Module ModuleState)
pushButton network = go (Seq.singleton $ Signal "" Low "broadcaster")
  where
    go Seq.Empty states =
      return states
    go (signal@(Signal _ pulse dest) :<| signals) states = do
      modify (signal :)
      case states !? dest of
        Nothing ->
          go signals states
        Just state -> do
          let (state', pulse') = process state signal
          let output = [Signal dest lowHigh m | (Just lowHigh) <- [pulse'], m <- network ! dest]
          go (signals >< Seq.fromList output) (Map.insert dest state' states)

partOne :: Map Module [Module] -> Map Module ModuleState -> Int
partOne network states = low * high
  where
    (_, signals) = runState (iterate (>>= pushButton network) (return states) !! 1000) []
    (length -> low, length -> high) = partition isLow signals
    isLow (Signal _ Low _) = True
    isLow _ = False

partTwo :: Map Module [Module] -> Map Module ModuleState -> Int
partTwo network states = foldl1 lcm $ map (countPulses states) conditions
  where
    countPulses states sig
      | sig `elem` signals = 1
      | otherwise = 1 + countPulses states' sig
      where
        (states', signals) = runState (pushButton network states) []

    conditions =
      -- Hardcoded for this particular input. Conjunction "dh" must have High
      -- pulse on all inputs in order to send Low pulse to "rx".
      [ Signal "tr" High "dh"
      , Signal "xm" High "dh"
      , Signal "dr" High "dh"
      , Signal "nh" High "dh"
      ]

main :: IO ()
main = do
  (network, states) <- parse <$> TextIO.readFile "input"
  putStrLn $ "Part One: " ++ show (partOne network states)
  putStrLn $ "Part Two: " ++ show (partTwo network states)
