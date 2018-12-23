{-# LANGUAGE LambdaCase #-}

module Main where

import System.Process 
import           Control.Concurrent           (forkIO, killThread, threadDelay, myThreadId)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar      (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan)
import           Control.Exception            (bracket, bracket_)
import           Control.Monad                (when, void)
import           Data.Bits                    (bit, (.|.))
import qualified Data.ByteString              as B
import           Data.Traversable             (for)
import           Listener
import qualified System.HID                   as HID
import           Text.Read                    (readMaybe)
import           Wiimote
import Command 
import Control.Exception (catch, throwIO, throwTo, ErrorCall(..))

main :: IO ()
main = do
    HID.init
    mainLoop 

mainLoop :: IO ()
mainLoop = do 
    listWiimoteSerials >>= \case 
        [] -> hammer . Notify $ "No wiimotes found"
        serials -> do
            let serial = head serials 
            catch 
                (withWiimote serial eventHandler) 
                (\e -> case (e :: WiimoteException) of 
                    _ -> do 
                        hammer $ Notify "Wiimote disconnected")
            -- serial <- selectOne serials
    --         me <- myThreadId 
    --         (withWiimote serial eventHandler) `catch` \e -> case (e :: WiimoteException) of 
    --                 HarshShutdown -> putStrLn "Harsh Shutdown" 
    --                 WiimoteNotFound -> putStrLn "Didn't find that one"
    -- putStrLn "Try again? [y/n]"
    -- getLine >>= \case 
    --     "y" -> mainLoop 
    --     _ -> void HID.exit

data HammerSpoonCmd 
    = KeyDown String
    | KeyUp String
    | Notify String 

hammer :: HammerSpoonCmd -> IO () 
hammer (KeyDown k) = do 
    void $ createProcess (proc "open" ["-g", "hammerspoon://wiimote?keydown=" ++ k])
hammer (KeyUp k) = do 
    void $ createProcess (proc "open" ["-g", "hammerspoon://wiimote?keyup=" ++ k])
hammer (Notify msg) = do 
    void $ createProcess (proc "open" ["-g", "hammerspoon://wiimote?notify=" ++ msg])

-- TODO allow a try again option
selectOne :: (Show s) => [s] -> IO s
selectOne values = do
    let options = zip [1..] values
    for options $ \(num, value) -> do
        putStr $ "(" ++ show num ++ ") "
        putStrLn . show $ value
    n <- getNumberInRange (1, length options)
    pure $ values !! (n - 1)

getNumberInRange :: (Int, Int) -> IO Int
getNumberInRange (min, max) = if min == max then pure min else loop where
    loop = do
        putStrLn $ "Pick an option in [" ++ show min ++ ".." ++ show max ++ "]."
        num <- getLine
        case readMaybe num of
            Nothing -> do
                putStrLn $ "\"" ++ num ++ "\" is not a number."
                loop
            Just n -> do
                if n <= max && n >= min
                    then pure n
                    else do
                        putStrLn $ "\"" ++ num ++ "\" is outside of the range."
                        loop

-- can this be reworked to allow an arbitrary monad in the response?
-- or at least some sort of app monad? That way the wiimoteRef can be
-- hidden away in a reader monad.

bindings = 
    [ (DpadLeft, "left")
    , (DpadRight, "right")
    , (DpadDown, "down")
    , (DpadUp, "up")]

-- TODO maybe consider having the wiimote state alongside the chan in the WiimoteRef
-- Would there be any problems where the following happens:
-- * A and B are pressed
-- * event handler handles A pressed, checks state to see if b is also pressed, then does things
-- * event handler handles B pressed (separately)
eventHandler wiimoteRef (Pressed btn) = do 
    case lookup btn bindings of 
        Nothing -> pure ()
        Just keyCode -> 
            hammer $ KeyDown keyCode
-- eventHandler wiimoteRef (Released btn) = do 
--     case lookup btn bindings of 
--         Nothing -> pure () 
--         Just keyCode -> 
--             hammer $ KeyUp keyCode 
    -- | isDirectional btn = do 
    --     -- bPressed <- b . buttons <$> refState wiimoteRef 
    --     when (btn == DpadLeft || btn == DpadRight) $ do 
    --         let dir = case btn of 
    --                 DpadLeft -> "left"
    --                 DpadRight -> "right"
    --         hammer $ Switch dir
-- eventHandler wiimoteRef (Pressed btn) = do
--     putStrLn $ show btn ++ " was pressed!"
-- eventHandler wiimoteRef (Released btn) = do
--     putStrLn $ show btn ++ " was released!"
-- eventHandler wiimoteRef Disconnected = do
--     hammer . Notify $ "Disconnected"
    -- enumerateWiimotes wiimoteRef 

eventHandler wiimoteRef (Connected serial) = do
    setLEDs wiimoteRef $ LEDState (False, True, True, False)
    -- putStrLn $ "Connected to " ++ serial
    hammer . Notify $ "Connected to " ++ serial

-- eventHandler wiimoteRef ReconnectFailed = do
--     putStrLn "Reconnect failed"

-- eventHandler wiimoteRef (Enumeration serials) = do
--     if length serials > 0
--         then do
--             serial <- selectOne serials
--             tryReconnect wiimoteRef serial
--         else do
--             putStrLn "No wiimotes found... Checking again in a second..."
--             threadDelay $ 10^6
--             enumerateWiimotes wiimoteRef

eventHandler _ _ = pure ()