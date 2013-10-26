import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch)

server = "irc.freenode.net"
port = 6667
chan = "#bj"
nick = "BJBOT"

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

main :: IO ()
main = bracket connect disconnect loop
  where
      disconnect = hClose . socket
      loop st    = catch (runReaderT run st) (\(SomeException _) -> return ())

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
    write "NICK" nick
    write "USER" (nick++" 0 * :bj bot")
    write "JOIN" chan
    forever $ do
        sock <- asks socket
        (user, command, channel, payload) <- readLine sock
        io (putStrLn user)
        io (putStrLn command)
        io (putStrLn channel)
        io (putStrLn payload)
  where forever a = a >> forever a

readLine :: Handle -> Net (String, String, String, String)
readLine h = do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    let s' = drop 1 s
        (front, back) = span (/= ':') s'
        back' = drop 1 back
        user : command : channel : _ = words front
    return (user, command, channel, back')

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval _ = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO
