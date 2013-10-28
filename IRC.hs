module IRC
( connect
, close
, run
, Source(..)
, getNick
, Net
, SockState(..)
, putClientState
, IRCLine(..)
, Command(..)
, privmsg
, pong
, io
, isInteger
) where

import Network
import System.IO
import Text.Printf
import Control.Monad.State


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

type Net a = StateT (SockState a) IO

putClientState :: a -> Net a a
putClientState g = do
    s <- get
    put (s { clientState = g })
    return g

data SockState a = SockState { socket :: Handle
                             , clientState :: a
                             }

connect :: String -> Int -> a -> IO (SockState a)
connect server port cs = do
    printf "Connecting to %s ..." server
    hFlush stdout
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    putStrLn "done."
    return (SockState {socket = h, clientState = cs})

close :: SockState a -> IO ()
close s =
    hClose $ socket s

run :: (IRCLine -> Net a ()) -> String -> String -> Net a ()
run ircHandler nickname chan = do
    write "NICK" nickname
    write "USER" (nickname++" 0 * :bj bot")
    write "JOIN" chan
    forever $ do
        sock <- gets socket
        line <- readLine
        case command line of UNKNOWN _ -> io (print line)
                             _ -> ircHandler line
  where forever a = a >> forever a

data IRCLine = IRCLine {
      source  :: Source
    , command :: Command
    , payload :: String
    } deriving Show

write :: String -> String -> Net a ()
write s t = do
    h <- gets socket
    io $ hPrintf h "%s %s\r\n" s t

io :: IO a -> Net b a
io = liftIO

readLine :: Net a IRCLine
readLine = do
    h <- gets socket
    s <- init `fmap` io (hGetLine h)
    -- io (putStrLn s)
    return (parseIRC s)

data Source = NoSource
            | UserSource String String
            | ServerSource String deriving Show
instance Eq Source where
    NoSource == NoSource = True
    UserSource _ sock1 == UserSource _ sock2 = sock1 == sock2
    ServerSource sock1 == ServerSource sock2 = sock1 == sock2
getNick :: Source -> String
getNick (UserSource nick  _) = nick
getNick _ = ""

parseIRC :: String -> IRCLine
parseIRC (':' : l) =
    let (src, l') = span (/= ' ') l
        source' = split (== '!') src
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

split :: (a -> Bool) -> [a] -> [[a]]
split pred xs = case dropWhile pred xs of
                [] -> []
                xs' -> x : split pred xs''
                    where (x, xs'') = break pred xs'

isInteger :: String -> Bool
isInteger s =
    case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _ -> False

privmsg :: String -> String -> Net a ()
privmsg chan s = write "PRIVMSG" (chan ++ " :" ++ s)

pong :: String -> Net a ()
pong s = write "PONG" (":" ++ s)

