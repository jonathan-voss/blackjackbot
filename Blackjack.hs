module Blackjack
( Card (..)
, Hand (..)
, listJoin
, formatCards
, isBlackjack
, Player (..)
, Game (..)
, newGame
, dealerPlayGame
, getResults
, dealGame
, hitCurrentTurn
, standCurrentPlayer
, turnIsOver
, moveNextPlayer
, roundIsOver
, getCurrentScore
, playerCount
, hasTurn
, removePlayer
, getPlayer
, emptyPlayer
, addPlayer
, splitCurrentPlayer
, doubleCurrentPlayer
, setWager
, playerWager
, fourdecks
, reshuffleLen
, shuffle
) where

import Data.List
import Data.Array.IO
import System.Random
import Control.Monad

defaultWager = 4
initialCash = 100

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Enum, Show)

data Card = Card {
      rank :: Integer
    , suit :: Suit
    }

instance Show Card where
    show card =
        let r = case rank card of
                1 -> "A"
                11 -> "J"
                12 -> "Q"
                13 -> "K"
                x -> show x
            s = case suit card of
                Spades -> "S"
                Hearts -> "H"
                Diamonds -> "D"
                Clubs -> "C"
        in r ++ s

data Hand = Hand {
      cards :: [Card]
    , wager :: Integer
    }

formatCards :: [Card] -> String
formatCards cards =
    "[" ++ listJoin " " (map show .reverse $ cards) ++ " (" ++ (show . scoreHand $ cards) ++ ")]"

listJoin :: [a] -> [[a]] -> [a]
listJoin sep (x:xs) =
    x ++ concat (map (sep ++) xs)
listJoin sep [] = []

scoreHand :: [Card] -> Integer
scoreHand cards =
    let ranks = [scoreCard c | c <- cards]
        total = sum ranks
    in subtractAces total cards
  where
    subtractAces :: Integer -> [Card] -> Integer
    subtractAces score (c:cs) =
        if score > 21 then
            let score' = if rank c == 1 then score - 10
                        else score
            in subtractAces score' cs
        else score
    subtractAces score [] =
        score

scoreCard :: Card -> Integer
scoreCard c =
    let r = rank c in
    if r == 1 then 11
    else if 11 <= r && r <= 13 then 10
    else r

isBlackjack :: [Card] -> Bool
isBlackjack [c1,c2] | scoreCard c1 == 11 && scoreCard c2 == 10 = True
isBlackjack [c1,c2] | scoreCard c1 == 10 && scoreCard c2 == 11 = True
isBlackjack _ = False

splitHand :: Hand -> [Card] -> Maybe (Hand, Hand, [Card])
splitHand hand deck =
    case cards hand of
    [c1, c2] | scoreCard c1 == scoreCard c2 -> do
        (hand1, deck') <- hitHand (hand{cards=[c1]}) deck
        (hand2, deck'') <- hitHand (hand{cards=[c2]}) deck'
        return (hand1, hand2, deck'')
    _ -> Nothing

hit :: [Card] -> [Card] -> Maybe ([Card],[Card])
hit hand (c:cs) = Just (c:hand, cs)
hit hand [] = Nothing

hitHand :: Hand -> [Card] -> Maybe (Hand, [Card])
hitHand hand deck = do
    (cards', deck') <- if scoreHand (cards hand) > 21 then Nothing
                      else hit (cards hand) deck
    return (Hand{cards=cards', wager=wager hand}, deck')

dealerPlay :: [Card] -> [Card] -> Maybe ([Card], [Card])
dealerPlay hand deck =
    if scoreHand hand < 17 then
        case hit hand deck of
        Just (hand, deck) -> dealerPlay hand deck
        Nothing -> Nothing
    else Just (hand, deck)

dealerPlayGame :: Game a -> Game a
dealerPlayGame game =
    case dealerPlay (dealerCards game) (gameDeck game) of
    Just (dc, deck) -> game{dealerCards=dc, gameDeck=deck}
    Nothing -> game

data Game a = Game {
      gameDeck          :: [Card]
    , players           :: [Player a]
    , finishedPlayers   :: [Player a]
    , dealerCards       :: [Card]
    } deriving Show

data Player a = Player {
      hands         :: [Hand]
    , playedHands   :: [Hand]
    , cash          :: Integer
    , baseWager     :: Integer
    , playerID      :: a
    } deriving Show

instance Show Hand where
    show hand = formatCards $ cards hand

instance Eq m => Eq (Player m) where
    p1 == p2 = playerID p1 == playerID p2

newGame :: [Card] -> Game a
newGame deck = Game {
    gameDeck = deck
  , players = []
  , finishedPlayers = []
  , dealerCards = []
  }

playerCount :: Game a -> Int
playerCount game = length (players game) + length (finishedPlayers game)

addPlayer :: Eq a => Game a -> a -> Game a
addPlayer g id =
    if not (isPlayer g id) then
        let p = emptyPlayer id in
        g { finishedPlayers = (p : finishedPlayers g) }
    else g

isPlayer :: Eq a => Game a -> a -> Bool
isPlayer game id =
    let p = emptyPlayer id in
    case (elemIndex p (players game), elemIndex p (finishedPlayers game)) of
    (Nothing, Nothing) -> False
    (_, _) -> True

getPlayer :: Eq a => Game a -> a -> Maybe (Player a)
getPlayer game id =
    let p = emptyPlayer id in
    case (elemIndex p (players game), elemIndex p (finishedPlayers game)) of
    (Nothing, Nothing) -> Nothing
    (Just idx, _) -> Just (players game !! idx)
    (_, Just idx) -> Just (finishedPlayers game !! idx)


emptyPlayer :: Eq a => a -> Player a
emptyPlayer id = Player { hands = []
                        , playedHands = []
                        , cash = initialCash
                        , baseWager = defaultWager
                        , playerID = id
                        }

hasTurn :: Eq a => Game a -> a -> Bool
hasTurn game id =
    case players game of
    (p:ps) -> id == playerID p
    [] -> False

roundIsOver :: Game a -> Bool
roundIsOver game =
    case players game of
    (x:xs) -> False
    [] -> True


turnIsOver :: Game a -> Bool
turnIsOver game =
    case players game of
    (p:_) -> case hands p of
             (_:_) -> False
             [] -> True
    [] -> True

removePlayer :: Eq a => Game a -> Player a -> Game a
removePlayer g p =
    let remove = filter (/= p) in
    g { players = remove (players g), finishedPlayers = remove (finishedPlayers g) }

setWager :: Eq a => Game a -> a -> Integer -> Game a
setWager game id amt =
    game { players = map (updateWager id) $ players game
         , finishedPlayers = map (updateWager id) $ finishedPlayers game
         }
  where
    updateWager :: Eq a => a -> Player a -> Player a
    updateWager id p =
        if playerID p == id && cash p >= amt && amt > 0 then 
            p { baseWager = amt }
        else p


dealGame :: Game a -> Maybe (Game a)
dealGame gs = do
    let ps = reverse (finishedPlayers gs) ++ players gs
    let g = gs{players = map (\p -> p{hands=[], playedHands=[]}) ps, finishedPlayers = []}
    (ps, deck) <- foldr dealWithMaybe (Just ([], gameDeck g)) (players g)
    (dealers, deck') <- deal deck
    return (g{ players = ps, gameDeck = deck', dealerCards=dealers })
  where
    dealWithMaybe :: Player a -> Maybe ([Player a], [Card]) -> Maybe ([Player a], [Card])
    dealWithMaybe p (Just (players, deck)) = do
        (p', deck') <- dealPlayer p deck
        return (p' : players, deck')
    dealWithMaybe p Nothing = Nothing

deal :: [Card] -> Maybe ([Card],[Card])
deal (c1:c2:cs) = Just ([c2, c1], cs)
deal _ = Nothing

dealPlayer :: Player a -> [Card] -> Maybe (Player a, [Card])
dealPlayer p deck = do
    (cs, deck') <- deal deck
    let hand = Hand{cards=cs, wager=baseWager p}
    return (p{hands=[hand], playedHands=[]}, deck')

hitPlayer :: Player a -> [Card] -> Maybe (Player a, [Card])
hitPlayer p deck =
    case hands p of
    (h:hs) -> do
        (h', deck') <- hitHand h deck
        return (p{hands=(h':hs)}, deck')
    [] -> Nothing

hitCurrentTurn :: Game a -> Maybe (Game a)
hitCurrentTurn game =
    case players game of
    (p:ps) -> do
        (p', deck) <- hitPlayer p (gameDeck game)
        return (game {players=(p':ps), gameDeck=deck})
    [] -> Nothing

splitPlayer :: Player a -> [Card] -> Maybe (Player a, [Card])
splitPlayer p deck =
    case hands p of
    (h:hs) -> do
        (h1,h2,deck') <- splitHand h deck
        return (p{hands=(h1:h2:hs)}, deck')
    [] -> Nothing

splitCurrentPlayer :: Game a -> Game a
splitCurrentPlayer game =
    case players game of
    (p:ps) -> case splitPlayer p (gameDeck game) of
              Just (p, deck) -> game {players = p : ps, gameDeck = deck}
              Nothing -> game
    [] -> game

getResults :: Game a -> (Game a, [(Player a, Integer)])
getResults game =
    let allPlayers = reverse (players game) ++ finishedPlayers game
        results = map (flip getPlayerResult $ dealerCards game) allPlayers
        ps = fst . unzip $ results
    in (game{finishedPlayers=ps, players=[]}, results)

getPlayerResult :: Player a -> [Card] -> (Player a, Integer)
getPlayerResult player dealer =
    let net = sum . map valuate . playedHands $ player
        net' = net + (sum . map valuate . hands $ player) in
    (player{cash=cash player + net'}, net')
  where
    valuate :: Hand -> Integer
    valuate hand =
        case compareCards (cards hand) dealer of
        LT -> -(wager hand)
        EQ -> 0
        GT | isBlackjack $ cards hand -> (3 * wager hand) `div` 2
        GT -> wager hand

compareCards :: [Card] -> [Card] -> Ordering
compareCards h1 h2 =
    let s1 = scoreHand h1
        s2 = scoreHand h2 in
    if isBlackjack h1 then
        if isBlackjack h2 then EQ
        else GT
    else if isBlackjack h2 then LT
    else if s1 > 21 then LT
    else if s2 > 21 then GT
    else compare s1 s2

standCurrentPlayer :: Game a -> Game a
standCurrentPlayer g =
    case players g of
    (p:ps) ->
        g{players=standPlayer p : ps}
    [] -> g

standPlayer :: Player a -> Player a
standPlayer p =
    case hands p of
    (h:hs) -> p{hands=hs, playedHands=(h:playedHands p)}
    [] -> p

doubleHand :: Hand -> [Card] -> Maybe (Hand, [Card])
doubleHand hand deck = do
    (hand', deck') <- hitHand hand deck
    return (Hand{cards=cards hand', wager=2*wager hand'}, deck')

doublePlayer :: Player a -> [Card] -> Maybe(Player a, [Card])
doublePlayer p deck =
    case hands p of
    (h:hs) -> do
        (h', deck') <- doubleHand h deck
        return (p{hands = h' : hs}, deck')
    [] -> Nothing

doubleCurrentPlayer :: Game a -> Game a
doubleCurrentPlayer g =
    case players g of
    (p:ps) -> case doublePlayer p (gameDeck g) of
              Just (p, d) -> g {players = p : ps, gameDeck = d}
              Nothing -> g
    [] -> g

moveNextPlayer :: Game a -> Game a
moveNextPlayer g =
    case players g of
    (p:ps) ->
        g{players=ps, finishedPlayers=(p:finishedPlayers g)}
    [] -> g

getCurrentScore :: Game a -> Maybe Integer
getCurrentScore game =
    case players game of
    (p:ps) -> case hands p of
              (h:hs) -> Just (scoreHand (cards h))
              [] -> Nothing
    [] -> Nothing

playerWager :: Player a -> Integer
playerWager p =
    let w = sum (map wager . hands $ p) in
    sum (map wager . playedHands $ p) + w



deck = [Card{rank=r,suit=s} | r <- [1..13], s <- [Spades .. Clubs]]
twodecks = concat [deck, deck]
fourdecks = concat [twodecks, twodecks]
reshuffleLen = 52 :: Int

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1,n) xs
