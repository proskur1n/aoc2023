import Data.List (elemIndex, partition, sort, sortBy)
import Data.Functor.Classes (liftCompare)
import Data.Ord (comparing, Down(..))

newtype Hand = Hand String deriving (Show, Eq)

newtype Bid = Bid Int deriving (Show, Eq, Ord)

data Type
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    deriving (Show, Eq, Ord)

parse :: String -> [(Hand, Bid)]
parse = map (toPair . words) . lines
  where
    toPair [hand, bid] = (Hand hand, Bid $ read bid)
    toPair _ = error "failed to parse input"

countChars :: String -> [(Char, Int)]
countChars = sortBy (comparing (Down . snd)) . go
  where
    go "" = []
    go str@(c:_) =
        let
            (same, other) = partition (== c) str
        in
            (c, length same) : go other

compareCardValues :: [Char] -> Hand -> Hand -> Ordering
compareCardValues ordering (Hand h0) (Hand h1) =
    liftCompare (comparing (`elemIndex` ordering)) h0 h1

handToType :: Hand -> Type
handToType (Hand hand) =
    case map snd $ countChars hand of
        [5]          -> FiveOfAKind
        [4, 1]       -> FourOfAKind
        [3, 2]       -> FullHouse
        [3, 1, 1]    -> ThreeOfAKind
        [2, 2, 1]    -> TwoPair
        [2, 1, 1, 1] -> OnePair
        _            -> HighCard

solve :: (Hand -> Hand -> Ordering) -> [(Hand, Bid)] -> Int
solve cmp = sum . zipWith (\i (_, Bid bid) -> i * bid) [1..] . sortBy (\(h0, _) (h1, _) -> cmp h0 h1)

partOne :: [(Hand, Bid)] -> Int
partOne = solve cmp
  where
    cmp :: Hand -> Hand -> Ordering
    h0 `cmp` h1 =
        case comparing handToType h0 h1 of
            EQ  -> compareCardValues "23456789TJQKA" h0 h1
            ord -> ord

partTwo :: [(Hand, Bid)] -> Int
partTwo = solve cmp
  where
    cmp :: Hand -> Hand -> Ordering
    h0 `cmp` h1 =
        case comparing (handToType . replaceJokers) h0 h1 of
            EQ  -> compareCardValues "J23456789TQKA" h0 h1
            ord -> ord

    replaceJokers :: Hand -> Hand
    replaceJokers (Hand hand) =
        let
            withoutJokers = filter (/= 'J') hand
            (mostCommon, _) = head $ countChars withoutJokers
        in
            if null withoutJokers then
                Hand "AAAAA"
            else
                Hand $ map (\c -> if c == 'J' then mostCommon else c) hand

main :: IO ()
main = readFile "input" >>= (print . partTwo . parse)
