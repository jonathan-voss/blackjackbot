import Data.Maybe
import Network
import System.IO
import Control.Monad.State
import Text.Printf
import Blackjack

server = "avaraline.net"
port = 6667
chan = "#bj"
nickname = "BJBOT"

playerToString :: Player Source -> String
playerToString p =
    let playedS = map show . playedHands $ p
        handsS = case map show . hands $ p of
                 (h:hs) -> ("[" ++ h ++ "]") : hs
                 [] -> []
        w = playerWager p
        allHands = listJoin " " (reverse playedS ++ handsS)
    in getNick (playerID p) ++ " $" ++ show w ++ " ($" ++ show (cash p) ++ ") " ++ allHands

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
             | PART String
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

showAllPlayers :: [Player Source] -> Net ()
showAllPlayers (p:ps) = do
    privmsg $ playerToString p
    showAllPlayers ps
showAllPlayers [] = return ()

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
    cards <- if (length $ gameDeck g) < reshuffleLen then do
                privmsg "reshuffling deck"
                io $ shuffle fourdecks
             else return (gameDeck g)
    g <- putGameState $ g{gameDeck=cards}
    case dealGame g of
        Just g -> do
            putGameState g
            if isBlackjack $ dealerCards g then do
                showAllPlayers (players g ++ finishedPlayers g)
                doFinishRound
                doStartGame
            else do
                doShowDealerCard
                doShowPlayerCards
                doAutoStands
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
            doAutoStands
    else do
        doShowPlayerCards
        doAutoStands

doAutoStands :: Net ()
doAutoStands = do
    gs <- gets gameState
    let score = getCurrentScore gs
    when (isJust score && fromJust score >= 21) $  do
        doStand
        doAutoStands

doLeave :: Source -> Net ()
doLeave source = do
    gs <- gets gameState
    when (playerCount gs > 1 && (hasTurn gs $ source)) $ do
        doStand
    gs <- gets gameState
    putGameState $ removePlayer gs (emptyPlayer $ source)
    return ()

ircHandler :: IRCLine -> Net ()
ircHandler line =
    case command line of
    PING ->
        pong $ payload line
    PART channel ->
        doLeave $ source line
    PRIVMSG dest ->
        case words (payload line) of
        ["join"] -> do
            gs <- gets gameState
            putGameState $ addPlayer gs (source line)
            when (playerCount gs == 0) doStartGame
        ["leave"] -> do
            doLeave $ source line
        ["wager", amt] -> do
            gs <- gets gameState
            when (isInteger amt) $ do
                putGameState $ setWager gs (source line) (read amt)
                privmsg "that will take effect next round"
        ["hit"] -> do
            gs <- gets gameState
            when (hasTurn gs (source line)) $ do
                doHit
                doAutoStands
        ["stand"] -> do
            gs <- gets gameState
            when (hasTurn gs $ source line) $ do
                doStand
        ["double"] -> do
            gs <- gets gameState
            when (hasTurn gs $ source line) $ do
                gs <- putGameState $ doubleCurrentPlayer gs
                doShowPlayerCards
                doStand
        ["split"] -> do
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
            ["PING"] -> (PING, [])
            ["PART", channel] -> (PART channel, [])
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

pong :: String -> Net ()
pong s = write "PONG" (":" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    io $ hPrintf h "%s %s\r\n" s t

io :: IO a -> Net a
io = liftIO
