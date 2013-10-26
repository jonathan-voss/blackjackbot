import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch)
import Data.Array.IO
import System.Random

server = "irc.freenode.net"
port = 6667
chan = "#bj"
nickname = "BJBOT"

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Enum, Show)
data Card = Card { 
      rank :: Integer
    , suit :: Suit
    } deriving Show

data Hand = Hand {
      cards :: [Card]
    , wager :: Integer
    }
data Player = Player {
      hands :: [Hand]
    , playedHands :: [Hand]
    , cash :: Integer
    , baseWager :: Integer
    }

deck = [Card{rank=r,suit=s} | r <- [1..13], s <- [Spades .. Clubs]]

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
            in subtractAces score cs
        else score
        
dealerPlay :: [Card] -> [Card] -> Maybe ([Card], [Card])
dealerPlay hand deck = 
    if scoreHand hand < 17 then
        case hit hand deck of
        Just (hand, deck) -> dealerPlay hand deck
        Nothing -> Nothing
    else Just (hand, deck)

deal :: [Card] -> Maybe ([Card],[Card])
deal (c1:c2:cs) = Just ([c2, c1], cs)
deal _ = Nothing

hit :: [Card] -> [Card] -> Maybe ([Card],[Card])
hit hand (c:cs) = Just (c:hand, cs)
hit hand [] = Nothing

splitHand :: [Card] -> [Card] -> Maybe ([Card], [Card], [Card])
splitHand [c1, c2] (d1:d2:deck) | rank c1 == rank c2 =
    Just ([c1, d1], [c2, d2], deck)
splitHand _ _ = Nothing


type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }
data Source = NoSource 
            | UserSource String String 
            | ServerSource String deriving Show

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
      source :: Source
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
main = bracket connect disconnect loop
  where
      disconnect = hClose . socket
      --loop st    = catch (runReaderT run st) (\(SomeException _) -> return ())
      loop st = runReaderT run st

connect :: IO Bot
connect = do
    printf "Connecting to %s ..." server 
    hFlush stdout
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    putStrLn "done."
    return (Bot h)

run :: Net ()
run = do
    write "NICK" nickname
    write "USER" (nickname++" 0 * :bj bot")
    write "JOIN" chan
    forever $ do
        sock <- asks socket
        line <- readLine sock
        case command line of UNKNOWN _ -> io (print line)
                             _ -> return ()
  where forever a = a >> forever a

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
            ["MODE", user] -> (MODE user, args)
            ["375", dest] -> (BEGINMOTD dest, [])
            ["372", dest] -> (CONTINUEMOTD dest, [])
            ["376", dest] -> (ENDMOTD dest, [])
            (s:args) | isInteger s -> (DONTCARE, [])
            f -> (UNKNOWN f, f) in
    IRCLine {source=NoSource, command=command', payload=drop 1 pload}

readLine :: Handle -> Net IRCLine
readLine h = do
    s <- init `fmap` io (hGetLine h)
    -- io (putStrLn s)
    return (parseIRC s)

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t

io :: IO a -> Net a
io = liftIO
