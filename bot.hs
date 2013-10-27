import Data.List
import Data.Maybe
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Exception
import Text.Printf
import Prelude hiding (catch)
import Data.Array.IO
import System.Random

server = "avaraline.net"
port = 6667
chan = "#bj"
nickname = "BJBOT"
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

instance Show Hand where
    show hand = formatCards $ cards hand

data Player a = Player {
      hands         :: [Hand]
    , playedHands   :: [Hand]
    , cash          :: Integer
    , baseWager     :: Integer
    , playerID      :: a
    } deriving Show

playerToString :: Player Source -> String
playerToString p =
    let playedS = map show . playedHands $ p
        handsS = case map show . hands $ p of
                 (h:hs) -> ("[" ++ h ++ "]") : hs
                 [] -> []
        w = playerWager p
        allHands = listJoin " " (reverse playedS ++ handsS)
    in getNick (playerID p) ++ " $" ++ show w ++ " ($" ++ show (cash p) ++ ") " ++ allHands

instance Eq m => Eq (Player m) where
    p1 == p2 = playerID p1 == playerID p2

data Game a = Game {
      gameDeck          :: [Card]
    , players           :: [Player a]
    , finishedPlayers   :: [Player a]
    , dealerCards       :: [Card]
    } deriving Show

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

emptyPlayer :: Eq a => a -> Player a
emptyPlayer id = Player { hands = []
                        , playedHands = []
                        , cash = initialCash
                        , baseWager = defaultWager
                        , playerID = id
                        }

playerWager :: Player a -> Integer
playerWager p =
    let w = sum (map wager . hands $ p) in
    sum (map wager . playedHands $ p) + w


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

isPlayer :: Eq a => Game a -> a -> Bool
isPlayer game id =
    let p = emptyPlayer id in
    case (elemIndex p (players game), elemIndex p (finishedPlayers game)) of
    (Nothing, Nothing) -> False
    (_, _) -> True

removePlayer :: Eq a => Game a -> Player a -> Game a
removePlayer g p =
    let remove = filter (/= p) in
    g { players = remove (players g), finishedPlayers = remove (finishedPlayers g) }

deck = [Card{rank=r,suit=s} | r <- [1..13], s <- [Spades .. Clubs]]
twodecks = concat [deck, deck]
fourdecks = concat [twodecks, twodecks]
reshuffleLen = 52

listJoin :: [a] -> [[a]] -> [a]
listJoin sep (x:xs) =
    x ++ concat (map (sep ++) xs)
listJoin sep [] = []

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

deal :: [Card] -> Maybe ([Card],[Card])
deal (c1:c2:cs) = Just ([c2, c1], cs)
deal _ = Nothing

dealPlayer :: Player a -> [Card] -> Maybe (Player a, [Card])
dealPlayer p deck = do
    (cs, deck') <- deal deck
    let hand = Hand{cards=cs, wager=baseWager p}
    return (p{hands=[hand], playedHands=[]}, deck')

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

hit :: [Card] -> [Card] -> Maybe ([Card],[Card])
hit hand (c:cs) = Just (c:hand, cs)
hit hand [] = Nothing

hitHand :: Hand -> [Card] -> Maybe (Hand, [Card])
hitHand hand deck = do
    (cards', deck') <- if scoreHand (cards hand) > 21 then Nothing
                      else hit (cards hand) deck
    return (Hand{cards=cards', wager=wager hand}, deck')

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

splitHand :: Hand -> [Card] -> Maybe (Hand, Hand, [Card])
splitHand hand deck =
    case cards hand of
    [c1, c2] | scoreCard c1 == scoreCard c2 -> do
        (hand1, deck') <- hitHand (hand{cards=[c1]}) deck
        (hand2, deck'') <- hitHand (hand{cards=[c2]}) deck'
        return (hand1, hand2, deck'')
    _ -> Nothing

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

getResults :: Game a -> (Game a, [(Player a, Integer)])
getResults game =
    let allPlayers = reverse (players game) ++ finishedPlayers game
        results = map (flip getPlayerResult $ dealerCards game) allPlayers
        ps = fst . unzip $ results
    in (game{finishedPlayers=ps, players=[]}, results)


standPlayer :: Player a -> Player a
standPlayer p =
    case hands p of
    (h:hs) -> p{hands=hs, playedHands=(h:playedHands p)}
    [] -> p

standCurrentPlayer :: Game a -> Game a
standCurrentPlayer g =
    case players g of
    (p:ps) ->
        g{players=standPlayer p : ps}
    [] -> g

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

type Net = StateT Bot IO
data Bot = Bot { socket :: Handle
               , gameState :: Game Source
               }

putGameState :: Game Source -> Net (Game Source)
putGameState g = do
    bot <- get
    put (bot { gameState = g })
    return g

data Source = NoSource
            | UserSource String String
            | ServerSource String deriving Show
getNick :: Source -> String
getNick (UserSource nick  _) = nick
getNick _ = ""

instance Eq Source where
    NoSource == NoSource = True
    UserSource _ sock1 == UserSource _ sock2 = sock1 == sock2
    ServerSource sock1 == ServerSource sock2 = sock1 == sock2

data Command = PING
             | PRIVMSG String
             | NOTICE String
             | JOIN String
             | MODE String
             | BEGINMOTD String
             | CONTINUEMOTD String
             | ENDMOTD String
             | UNKNOWN [String]
             | DONTCARE
             | QUIT deriving Show

data IRCLine = IRCLine {
      source  :: Source
    , command :: Command
    , payload :: String
    } deriving Show

isInteger :: String -> Bool
isInteger s =
    case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _ -> False

split :: (a -> Bool) -> [a] -> [[a]]
split pred xs = case dropWhile pred xs of
                [] -> []
                xs' -> x : Main.split pred xs''
                    where (x, xs'') = break pred xs'

main :: IO ()
main = do
    bot <- connect
    runStateT (run ircHandler) bot
    hClose $ socket bot

connect :: IO Bot
connect = do
    printf "Connecting to %s ..." server
    hFlush stdout
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    putStrLn "done."
    deck <- shuffle fourdecks
    return (Bot {socket = h, gameState = newGame deck})

run :: (IRCLine -> Net ()) -> Net ()
run ircHandler = do
    write "NICK" nickname
    write "USER" (nickname++" 0 * :bj bot")
    write "JOIN" chan
    forever $ do
        sock <- gets socket
        line <- readLine
        case command line of UNKNOWN _ -> io (print line)
                             _ -> ircHandler line
  where forever a = a >> forever a

doStartNextTurn :: Net (Bool)
doStartNextTurn = do
    g <- gets gameState
    case players g of
        (p:ps) -> do
            putGameState (g { players = ps, finishedPlayers = p : finishedPlayers g })
            return True
        [] ->
            return False

doShowPlayerCards :: Net ()
doShowPlayerCards = do
    g <- gets gameState
    case players g of
        (p:ps) ->
            privmsg $ playerToString p
        [] -> privmsg "uhh no players??"

doShowDealerCard :: Net ()
doShowDealerCard = do
    g <- gets gameState
    case reverse $ dealerCards g of
        (c1:_) ->
            privmsg $ ("I have " ++ show c1 ++ " _")
        [] ->
            privmsg $ "dealer doesn't have cards :X"

doFinishRound :: Net ()
doFinishRound = do
    g <- gets gameState
    g' <- putGameState $ dealerPlayGame g
    let (g'', ps) = getResults g'
    putGameState g''
    privmsg $ ("I have " ++ formatCards (dealerCards g'))
    privmsg $ (listJoin " " (map genreport ps))
  where
    genreport :: (Player Source, Integer) -> String
    genreport (p, amt) =
        let s = if amt >= 0 then " won "
                else " lost " in
        getNick (playerID p) ++ s ++ (show $ abs amt) ++ "."


doStartGame :: Net ()
doStartGame = do
    g <- gets gameState
    cards <- io $ if (length $ gameDeck g) < reshuffleLen then shuffle fourdecks else return (gameDeck g)
    g <- putGameState $ g{gameDeck=cards}
    case dealGame g of
        Just g -> do
            putGameState g
            if isBlackjack $ dealerCards g then do
                doFinishRound
                doStartGame
            else do
                doShowDealerCard
                doShowPlayerCards
        Nothing -> privmsg "omg forgot to shuffle deck :("

doHit :: Net ()
doHit = do
    gs <- gets gameState
    let gm = hitCurrentTurn gs
    when (isJust gm) $ do
        putGameState (fromJust gm)
        return ()
    doShowPlayerCards
doStand :: Net ()
doStand = do
    gs <- gets gameState
    gs <- putGameState $ standCurrentPlayer gs
    if turnIsOver gs then do
        gs' <- putGameState $ moveNextPlayer gs
        if roundIsOver gs' then do
            doFinishRound
            doStartGame
        else do
            doShowDealerCard
            doShowPlayerCards
    else do
        doShowPlayerCards

doAutoStands :: Net ()
doAutoStands = do
    gs <- gets gameState
    let score = getCurrentScore gs
    when (isJust score && fromJust score >= 21) $  do
        doStand
        doAutoStands

ircHandler :: IRCLine -> Net ()
ircHandler line =
    case command line of
    PRIVMSG dest ->
        case words (payload line) of
        ("!join":_) -> do
            gs <- gets gameState
            putGameState $ addPlayer gs (source line)
            when (playerCount gs == 0) doStartGame
        ("!hit":_) -> do
            gs <- gets gameState
            when (hasTurn gs (source line)) $ do
                doHit
                doAutoStands
        ("!stand":_) -> do
            gs <- gets gameState
            when (hasTurn gs $ source line) $ do
                doStand
        ("!double":_) -> do
            gs <- gets gameState
            when (hasTurn gs $ source line) $ do
                gs <- putGameState $ doubleCurrentPlayer gs
                doShowPlayerCards
                doStand
        ("!split":_) -> do
            gs <- gets gameState
            when (hasTurn gs $ source line) $ do
                gs <- putGameState $ splitCurrentPlayer gs
                doShowPlayerCards
                doAutoStands
        _ -> return ()
    _ -> return ()

parseIRC :: String -> IRCLine
parseIRC (':' : l) =

    let (src, l') = span (/= ' ') l
        source' = Main.split (== '!') src
        source'' =  case source' of
                        [nick, addr] -> UserSource nick addr
                        [addr] -> ServerSource addr
        parsed = parseIRC . drop 1 $  l' in
    IRCLine {source=source'', command=(command parsed), payload=(payload parsed)}
parseIRC l =
    let (c, pload) = span (/= ':') l
        (command', args) = case words c of
            --("PING":args) -> (PING, args)
            ("PRIVMSG":dest:args) -> (PRIVMSG dest, args)
            ("NOTICE":dest:args) -> (NOTICE dest, args)
            ["JOIN", channel] -> (JOIN channel, args)
            ["JOIN"] -> (JOIN (head args), tail args)
            ["MODE", user] -> (MODE user, args)
            ["375", dest] -> (BEGINMOTD dest, [])
            ["372", dest] -> (CONTINUEMOTD dest, [])
            ["376", dest] -> (ENDMOTD dest, [])
            (s:args) | isInteger s -> (DONTCARE, [])
            f -> (UNKNOWN f, f) in
    IRCLine {source=NoSource, command=command', payload=drop 1 pload}

readLine :: Net IRCLine
readLine = do
    h <- gets socket
    s <- init `fmap` io (hGetLine h)
    -- io (putStrLn s)
    return (parseIRC s)

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    io $ hPrintf h "%s %s\r\n" s t

io :: IO a -> Net a
io = liftIO
