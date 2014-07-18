module IRC
( connect
, auth
, readLine
, close
, Source(..)
, getNick
, IRCLine(..)
, Command(..)
, privmsg
, pong
, isInteger
) where

import Network
import System.IO
import System.IO.Unsafe
import Text.Printf

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

connect :: String -> Int -> IO Handle
connect server port = do
    printf "Connecting to %s ..." server
    hFlush stdout
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    putStrLn "done."
    return h

close = hClose

handlePing :: Handle -> IO ()
handlePing sock = do
    line <- readLine sock
    case command line of PING -> pong  sock (payload line)
                         PRIVMSG s -> return ()
                         _ -> handlePing sock

auth :: Handle -> String -> String -> IO ()
auth sock nickname chan = do
    write sock "NICK" nickname
    write sock "USER" (nickname++" 0 * :bj bot")
    --handlePing sock
    write sock "JOIN" chan

data IRCLine = IRCLine {
      source  :: Source
    , command :: Command
    , payload :: String
    } deriving Show

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t

readLine :: Handle -> IO IRCLine
readLine h = do
    s <- init `fmap` hGetLine h

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
        (command', rest) = case words c of
            ["PING"] -> (PING, [])
            ["PART", channel] -> (PART channel, [])
            ("PRIVMSG":dest:args) -> (PRIVMSG dest, args)
            ("NOTICE":dest:args) -> (NOTICE dest, args)
            ["JOIN", channel] -> (JOIN channel, [])
            ["JOIN"] -> (JOIN (drop 1 pload), [])
            ["MODE", user] -> (MODE user, [])
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

privmsg :: Handle -> String -> String -> IO ()
privmsg h chan s = write h "PRIVMSG" (chan ++ " :" ++ s)

pong :: Handle -> String -> IO ()
pong h s = write h "PONG" (":" ++ s)

