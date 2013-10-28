import Data.Maybe
import Text.Printf
import Control.Monad.State
import System.IO
import Blackjack
import IRC

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

main :: IO ()
main = do
    deck <- shuffle fourdecks
    bot <- connect server port (newGame deck)
    runStateT (run ircHandler nickname chan) bot
    close bot


doStartNextTurn :: Net (Game Source) Bool
doStartNextTurn = do
    g <- gets clientState
    case players g of
        (p:ps) -> do
            putClientState (g { players = ps, finishedPlayers = p : finishedPlayers g })
            return True
        [] ->
            return False

doShowPlayerCards :: Net (Game Source) ()
doShowPlayerCards = do
    g <- gets clientState
    case players g of
        (p:ps) ->
            privmsg chan $ playerToString p
        [] -> privmsg chan "uhh no players??"

doShowDealerCard :: Net (Game Source) ()
doShowDealerCard = do
    g <- gets clientState
    case reverse $ dealerCards g of
        (c1:_) ->
            privmsg chan $ ("I have " ++ show c1 ++ " _")
        [] ->
            privmsg chan $ "dealer doesn't have cards :X"

showAllPlayers :: [Player Source] -> Net (Game Source) ()
showAllPlayers (p:ps) = do
    privmsg chan $ playerToString p
    showAllPlayers ps
showAllPlayers [] = return ()

doFinishRound :: Net (Game Source) ()
doFinishRound = do
    g <- gets clientState
    g' <- putClientState $ dealerPlayGame g
    let (g'', ps) = getResults g'
    putClientState g''
    privmsg chan $ ("I have " ++ formatCards (dealerCards g'))
    privmsg chan $ (listJoin " " (map genreport ps))
  where
    genreport :: (Player Source, Integer) -> String
    genreport (p, amt) =
        let s = if amt >= 0 then " won "
                else " lost " in
        getNick (playerID p) ++ s ++ (show $ abs amt) ++ "."


doStartGame :: Net (Game Source) ()
doStartGame = do
    g <- gets clientState
    cards <- if (length $ gameDeck g) < reshuffleLen then do
                privmsg chan "reshuffling deck"
                io $ shuffle fourdecks
             else return (gameDeck g)
    g <- putClientState $ g{gameDeck=cards}
    case dealGame g of
        Just g -> do
            putClientState g
            if isBlackjack $ dealerCards g then do
                showAllPlayers (players g ++ finishedPlayers g)
                doFinishRound
                doStartGame
            else do
                doShowDealerCard
                doShowPlayerCards
                doAutoStands
        Nothing -> privmsg chan "omg forgot to shuffle deck :("

doHit :: Net (Game Source) ()
doHit = do
    gs <- gets clientState
    let gm = hitCurrentTurn gs
    when (isJust gm) $ do
        putClientState (fromJust gm)
        return ()
    doShowPlayerCards

doStand :: Net (Game Source) ()
doStand = do
    gs <- gets clientState
    gs <- putClientState $ standCurrentPlayer gs
    if turnIsOver gs then do
        gs' <- putClientState $ moveNextPlayer gs
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

doAutoStands :: Net (Game Source) ()
doAutoStands = do
    gs <- gets clientState
    let score = getCurrentScore gs
    when (isJust score && fromJust score >= 21) $  do
        doStand
        doAutoStands

doLeave :: Source -> Net (Game Source) ()
doLeave source = do
    gs <- gets clientState
    when (playerCount gs > 1 && (hasTurn gs $ source)) $ do
        doStand
    gs <- gets clientState
    putClientState $ removePlayer gs (emptyPlayer $ source)
    return ()

ircHandler :: IRCLine -> Net (Game Source) ()
ircHandler line =
    case command line of
    PING ->
        pong $ payload line
    PART channel ->
        doLeave $ source line
    PRIVMSG dest ->
        case words (payload line) of
        ["join"] -> do
            gs <- gets clientState
            putClientState $ addPlayer gs (source line)
            when (playerCount gs == 0) doStartGame
        ["leave"] -> do
            doLeave $ source line
        ["wager", amt] -> do
            gs <- gets clientState
            when (isInteger amt) $ do
                putClientState $ setWager gs (source line) (read amt)
                privmsg chan "that will take effect next round"
        ["hit"] -> do
            gs <- gets clientState
            when (hasTurn gs (source line)) $ do
                doHit
                doAutoStands
        ["stand"] -> do
            gs <- gets clientState
            when (hasTurn gs $ source line) $ do
                doStand
        ["double"] -> do
            gs <- gets clientState
            when (hasTurn gs $ source line) $ do
                gs <- putClientState $ doubleCurrentPlayer gs
                doShowPlayerCards
                doStand
        ["split"] -> do
            gs <- gets clientState
            when (hasTurn gs $ source line) $ do
                gs <- putClientState $ splitCurrentPlayer gs
                doShowPlayerCards
                doAutoStands
        _ -> return ()
    _ -> return ()
