{-# LANGUAGE NoFieldSelectors #-}
import Text.Parsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Workflow = String

type MachinePart = [Int]

data Threshold
  = Threshold
  { rating :: Int
  , less :: Bool
  , threshold :: Int
  }

data Decision
  = Condition Decision Threshold Decision
  | Accept
  | Reject

type Interval = (Int, Int) -- both ends are inclusive

parseInput :: String -> (Map Workflow [(Maybe Threshold, Workflow)], [MachinePart])
parseInput = either (error . show) id . parse parser ""
  where
    parser = do
      workflows <- workflow `endBy` endOfLine
      endOfLine
      ratings <- rating `endBy` endOfLine
      eof
      return (Map.fromList workflows, ratings)

    rating =
      between (char '{') (char '}') ((property >> char '=' >> number) `sepBy` char ',')

    workflow = do
      name <- word
      rules <- between (char '{') (char '}') (rule `sepBy` char ',')
      return (name, rules)

    rule = do
      cond <- option Nothing (Just <$> try condition)
      nextWorkflow <- word
      return (cond, nextWorkflow)

    condition = do
      rating <- property
      less <- (True <$ char '<') <|> (False <$ char '>')
      threshold <- number
      char ':'
      return $ Threshold rating less threshold

    property = (0 <$ char 'x') <|> (1 <$ char 'm') <|> (2 <$ char 'a') <|> (3 <$ char 's')

    number = read <$> many1 digit

    word = many1 letter

buildDecisionTree :: Map Workflow [(Maybe Threshold, Workflow)] -> Decision
buildDecisionTree pipeline = build (pipeline Map.! "in")
  where
    build :: [(Maybe Threshold, Workflow)] -> Decision
    build ((Nothing, next):_) = switch next
    build ((Just threshold, next):rest) = Condition (build rest) threshold (switch next)

    switch :: Workflow -> Decision
    switch "A" = Accept
    switch "R" = Reject
    switch next = build (pipeline Map.! next)

accepted :: Decision -> MachinePart -> Bool
accepted decision part =
  case decision of
    Condition left threshold right ->
      if part `fulfills` threshold then
        accepted right part
      else
        accepted left part
    Accept -> True
    Reject -> False
  where
    fulfills :: MachinePart -> Threshold -> Bool
    fulfills part (Threshold rating less threshold)
      | less      = val < threshold
      | otherwise = val > threshold
      where val = part !! rating

countCombinations :: Decision -> [Interval] -> Int
countCombinations decision ratings =
  case decision of
    Condition left threshold@(Threshold n _ _) right ->
          countCombinations left (mapNth n (notHolds threshold) ratings)
          + countCombinations right (mapNth n (holds threshold) ratings)
    Accept ->
      product $ map intervalSize ratings
    Reject ->
      0
  where
    holds :: Threshold -> Interval -> Interval
    holds (Threshold _ less threshold) (a, b)
      | less      = (a, min (threshold - 1) b)
      | otherwise = (max (threshold + 1) a, b)

    notHolds :: Threshold -> Interval -> Interval
    notHolds (Threshold _ less threshold) (a, b)
     | less      = (max threshold a, b)
     | otherwise = (a, min threshold b)

    mapNth :: Int -> (a -> a) -> [a] -> [a]
    mapNth 0 fn (x:xs) = fn x : xs
    mapNth n fn (x:xs) = x : mapNth (n - 1) fn xs

    intervalSize :: Interval -> Int
    intervalSize (a, b) = max 0 (b - a + 1)

partOne :: Decision -> [MachinePart] -> Int
partOne decision = sum . concat . filter (accepted decision)

partTwo :: Decision -> Int
partTwo decision = countCombinations decision (replicate 4 (1, 4000))

main :: IO ()
main = do
  (pipeline, machineParts) <- parseInput <$> readFile "input"
  let tree = buildDecisionTree pipeline
  putStrLn $ "Part One: " ++ show (partOne tree machineParts)
  putStrLn $ "Part Two: " ++ show (partTwo tree)
