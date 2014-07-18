import Data.Maybe
import System.IO
import System.Random
import Control.Monad.Writer
import Blackjack
import IRC

server :: String
server = "avaraline.net"
port :: Int
port = 6667
chan :: String
chan = "#bj"
nickname :: String
nickname = "bjbot"


playerToString :: Player Source -> String
playerToString p =
    let playedS = map show . playedHands $ p
        handsS = case map show . hands $ p of
                 (h:hs) -> ("[" ++ h ++ "]") : hs
                 [] -> []
        w = playerWager p
        allHands = listJoin " " (reverse playedS ++ handsS)
    in getNick (playerID p) ++ " $" ++ show w ++ " ($" ++ show (cash p) ++ ") " ++ allHands

main :: IO ()
main = do
    h <- connect server port
    gen <- newStdGen
    let gs = newGame fourdecks gen
    auth h nickname chan
    let go gamestate = do
        ircline <- readLine h
        gs' <- ircHandler h gamestate ircline
        go gs'
    go gs
    close h

leave :: Game Source -> Source -> Writer [String] (Game Source)
leave gs playersource = do
    gs <- if playerCount gs > 1 && hasTurn gs playersource then
            stand gs
          else
            return gs
    return $ removePlayer gs (emptyPlayer $ playersource)

hit :: Game Source -> Writer [String] (Game Source)
hit gs = do
    gs <- return . fromMaybe gs . hitCurrentTurn $ gs
    showPlayerCards gs
    return gs

stand :: Game Source -> Writer [String] (Game Source)
stand gs = do
    gs <- return . standCurrentPlayer $ gs
    if turnIsOver gs then do
        gs <- return . moveNextPlayer $ gs
        if roundIsOver gs then do
            gs <- finishRound gs
            startGame gs
        else do
            showDealerCard gs
            showPlayerCards gs
            autoStands gs
    else do
        showPlayerCards gs
        autoStands gs

autoStands :: Game Source -> Writer [String] (Game Source)
autoStands gs =
    let score = getCurrentScore gs in 
    if (isJust score && fromJust score >= 21) then do
        gs <- stand gs 
        autoStands gs
    else
        return gs

finishRound :: Game Source -> Writer [String] (Game Source)
finishRound g = do
    g <- return $ dealerPlayGame g 
    (g, ps) <- return $ getResults g
    tell [ "I have " ++ formatCards (dealerCards g)
         , (listJoin " " (map genreport ps))
         ]
    return g
  where
    genreport :: (Player Source, Integer) -> String
    genreport (p, amt) =
        let s = if amt >= 0 then " won "
                else " lost " in
        getNick (playerID p) ++ s ++ (show $ abs amt) ++ "."

startGame :: Game Source -> Writer [String] (Game Source)
startGame g = do
    g <- if length (gameDeck g) < reshuffleLen then do
            tell ["reshuffling deck"]
            return $ shuffleDeck g fourdecks
         else return g
    case dealGame g of
        Just g ->
            if isBlackjack $ dealerCards g then do
                showAllPlayers (players g ++ finishedPlayers g)
                g <- finishRound g
                startGame g
            else do
                showDealerCard g
                showPlayerCards g
                autoStands g
        Nothing -> do
            tell ["omg forgot to shuffle deck :("]
            return g

showDealerCard :: Game Source -> Writer [String] ()
showDealerCard g =
    case reverse $ dealerCards g of
        (c1:_) ->
            tell ["I have " ++ show c1 ++ " _"]
        [] ->
            tell ["dealer doesn't have cards :X"]

showPlayerCards :: Game Source -> Writer [String] ()
showPlayerCards g = do
    case players g of
        (p:_) -> tell [playerToString p]
        [] -> tell ["uhh no players??"]

showAllPlayers :: [Player Source] -> Writer [String] ()
showAllPlayers gs = 
    mapM_ (\s -> tell [playerToString s]) gs

ircHandler :: Handle -> Game Source -> IRCLine -> IO (Game Source)
ircHandler h g line =
    case command line of
        PING -> do
            pong h $ payload line
            return g
        _ -> do
            let (gamestate, response) = (runWriter (process g line))
            mapM_ (\s -> privmsg h chan s) response
            return gamestate
            
process :: Game Source -> IRCLine -> Writer [String] (Game Source)
process gs line =
    case command line of 
        PART _ ->
            leave gs $ source line
        PRIVMSG dest ->
            if dest == chan then 
                case words (payload line) of
                    ["join"] -> do
                        gs <- return $ addPlayer gs (source line)
                        if playerCount gs == 1 then startGame gs
                        else return gs
                    ["whosturn"] -> do
                        let response = case getCurrentPlayer gs of
                                                Just p -> 
                                                    [getNick . playerID $ p]
                                                Nothing -> 
                                                    ["No one's turn"]
                        tell response
                        return gs
                    ["leave"] ->
                        leave gs $ source line
                    ["wager", amt] ->
                        let amt' = read amt in
                        case getPlayer gs (source line) of
                            Just p ->
                                if 0 < amt' && amt' <= cash p then do
                                    tell ["that will take effect next round"]
                                    return $ setWager gs (source line) amt'
                                else do
                                    tell ["nice try"]
                                    return gs
                            Nothing ->
                                return gs
                    ["hit"] ->
                        if hasTurn gs (source line) then do
                            gs <- hit gs 
                            autoStands gs
                        else return gs
                    ["stand"] ->
                        if hasTurn gs $ source line then
                            stand gs
                        else return gs
                    ["double"] ->
                        if hasTurn gs $ source line then do
                            gs <- return $ doubleCurrentPlayer gs 
                            showPlayerCards gs
                            stand gs
                        else return gs

                    ["split"] ->
                        if hasTurn gs $ source line then do
                            gs <- return $ splitCurrentPlayer gs
                            showPlayerCards gs
                            autoStands gs
                        else return gs
                    _ -> return gs
            else return gs
        _ -> return gs
