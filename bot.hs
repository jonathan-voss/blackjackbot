import Data.Maybe
import System.IO
import System.Random
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

leave :: Game Source -> Source -> (Game Source, [String])
leave gs playersource =
    let (gs', response) = 
         if playerCount gs > 1 && hasTurn gs playersource then
             stand gs
         else
             (gs, []) in 
    ((removePlayer gs' (emptyPlayer $ playersource)), response)

hit :: Game Source -> (Game Source, [String])
hit gs =
    let gs' = case hitCurrentTurn gs of
                 Just gamestate -> gamestate
                 Nothing -> gs in
    (gs', showPlayerCards gs')

stand :: Game Source -> (Game Source, [String])
stand gs =
    let gs' = standCurrentPlayer gs in
    if turnIsOver gs' then
        let gs'' = moveNextPlayer gs' in
        if roundIsOver gs'' then
            let (gs''', response) = finishRound gs''
                (gs'''', moreresponse) = startGame gs''' in
            (gs'''', response ++ moreresponse)
        else
            let dealerlines = showDealerCard gs''
                playerlines = showPlayerCards gs''
                (gs''', stands) = autoStands gs'' in
            (gs''', dealerlines ++ playerlines ++ stands)

    else
        let playerlines = showPlayerCards gs' in
        let (gs'', stands) = autoStands gs' in
        (gs'', playerlines ++ stands)

autoStands :: Game Source -> (Game Source, [String])
autoStands gs =
    let score = getCurrentScore gs in 
    if (isJust score && fromJust score >= 21) then
        let (gs', response) = stand gs 
            (gs'', moreresponse) = autoStands gs' in
        (gs'', response ++ moreresponse)
    else
        (gs, [])

finishRound :: Game Source -> (Game Source, [String])
finishRound g =
    let g' = dealerPlayGame g 
        (g'', ps) = getResults g'
        response = [ "I have " ++ formatCards (dealerCards g')
                , (listJoin " " (map genreport ps))
                ] in
    (g'', response)
  where
    genreport :: (Player Source, Integer) -> String
    genreport (p, amt) =
        let s = if amt >= 0 then " won "
                else " lost " in
        getNick (playerID p) ++ s ++ (show $ abs amt) ++ "."

startGame :: Game Source -> (Game Source, [String])
startGame g =
    let (g', response) = 
         if length (gameDeck g) < reshuffleLen then
             (shuffleDeck g fourdecks, "reshuffling deck")
         else (g, []) in
    case dealGame g' of
        Just game ->
            if isBlackjack $ dealerCards game then
                let showplayers = showAllPlayers (players game ++ finishedPlayers game)
                    (g'', finishround) = finishRound game
                    (g''', nextround) = startGame g'' in
                (g''', response : showplayers ++ finishround ++ nextround)
            else
                let dealerresponse = showDealerCard game in
                let playerresponse = showPlayerCards game in
                let (g'', stands) = autoStands game in
                (g'', response : dealerresponse ++ playerresponse ++ stands)
        Nothing ->
            (g', response : ["omg forgot to shuffle deck :("])

showDealerCard :: Game Source -> [String]
showDealerCard g =
    case reverse $ dealerCards g of
        (c1:_) ->
            ["I have " ++ show c1 ++ " _"]
        [] ->
            ["dealer doesn't have cards :X"]

showPlayerCards :: Game Source -> [String]
showPlayerCards g = do
    case players g of
        (p:_) -> [playerToString p]
        [] -> ["uhh no players??"]

showAllPlayers :: [Player Source] -> [String]
showAllPlayers ps =
    map playerToString ps

ircHandler :: Handle -> Game Source -> IRCLine -> IO (Game Source)
ircHandler h g line =
    case command line of
        PING -> do
            pong h $ payload line
            return g
        _ -> do
            let (gamestate, response) = process g line
            mapM_ (\s -> privmsg h chan s) response
            return gamestate
            
process :: Game Source -> IRCLine -> (Game Source, [String])
process gs line =
    case command line of 
        PART _ ->
            leave gs $ source line
        PRIVMSG dest ->
            if dest == chan then 
                case words (payload line) of
                    ["join"] ->
                        let gs' = addPlayer gs (source line) in
                        if playerCount gs' == 1 then startGame gs'
                        else (gs', [])
                    ["whosturn"] ->
                        case getCurrentPlayer gs of
                            Just p -> (gs, [getNick . playerID $ p])
                            Nothing -> (gs, ["No one's turn"])
                    ["leave"] ->
                        leave gs $ source line
                    ["wager", amt] ->
                        let amt' = read amt in
                        case getPlayer gs (source line) of
                            Just p ->
                                if 0 < amt' && amt' <= cash p then
                                    (setWager gs (source line) amt', ["that will take effect next round"])
                                else
                                    (gs, ["nice try"])
                            Nothing ->
                                (gs, [])
                    ["hit"] ->
                        if hasTurn gs (source line) then
                            let (gs', hitline) = hit gs 
                                (gs'', standsline) = autoStands gs' in
                            (gs'', hitline ++ standsline)
                        else (gs, [])
                    ["stand"] ->
                        if hasTurn gs $ source line then
                            stand gs
                        else (gs, [])
                    ["double"] ->
                        if hasTurn gs $ source line then
                            let gs' = doubleCurrentPlayer gs 
                                showline = showPlayerCards gs'
                                (gs'', standline) = stand gs' in
                            (gs'', showline ++ standline)
                        else (gs, [])

                    ["split"] ->
                        if hasTurn gs $ source line then
                            let gs' = splitCurrentPlayer gs
                                showline = showPlayerCards gs'
                                (gs'', standline) = autoStands gs' in
                            (gs'', showline ++ standline)
                        else (gs, [])
                    _ -> (gs, [])
            else (gs, [])
        _ -> (gs, [])
