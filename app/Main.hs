module Main where

import           Control.Concurrent           (forkIO, killThread, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar      (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan)
import           Control.Exception            (bracket, bracket_)
import           Control.Monad                (when)
import           Data.Bits                    (bit, (.|.))
import qualified Data.ByteString              as B
import           Data.Traversable             (for)
import           Listener
import qualified System.HID                   as HID
import           Text.Read                    (readMaybe)
import           Wiimote

main :: IO ()
main = do
    HID.init
    serials <- listWiimoteSerials
    case serials of
        [] -> putStrLn "No wiimotes found"
        serials -> do
            serial <- selectOne serials
            withAsync (withWiimote serial eventHandler) $ \done -> do 
                wait done
                putStrLn "This one shutdown"
                pure ()
            -- shutdown <- newEmptyMVar
            -- let startThread = forkIO $ bracket_
            --                     (pure ())
            --                     (putMVar shutdown True)
            --                     (withWiimote serial eventHandler)
            -- shutdown <- bracket
            --                 (startThread)
            --                 (killThread)
            --                 (\_ -> takeMVar shutdown) -- waits for thread to shutdown normally
            --                                           -- replace this with some sort of body.
            --                                           -- if an exception is raised, this makes
            --                                           -- sure to kill the listener loop
            -- pure ()

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

-- TODO maybe consider having the wiimote state alongside the chan in the WiimoteRef
-- Would there be any problems where the following happens:
-- * A and B are pressed
-- * event handler handles A pressed, checks state to see if b is also pressed, then does things
-- * event handler handles B pressed (separately)

eventHandler wiimoteRef (Pressed btn) = do
    putStrLn $ show btn ++ " was pressed!"
eventHandler wiimoteRef (Released btn) = do
    putStrLn $ show btn ++ " was released!"
eventHandler wiimoteRef Disconnected = do
    putStrLn "Disconnected from wiimote"
    -- enumerateWiimotes wiimoteRef

eventHandler wiimoteRef (Connected serial) = do
    setLEDs wiimoteRef $ LEDState (False, True, True, False)
    putStrLn $ "Connected to " ++ serial

eventHandler wiimoteRef ReconnectFailed = do
    putStrLn "Reconnect failed"
    -- tryReconnect wiimoteRef (serialNumber wiimoteRef)
eventHandler wiimoteRef (Enumeration serials) = do
    if length serials > 0
        then do
            serial <- selectOne serials
            tryReconnect wiimoteRef serial
        else do
            putStrLn "No wiimotes found... Checking again in a second..."
            threadDelay $ 10^6
            enumerateWiimotes wiimoteRef


-- eventHandler chan = do
--     Pressed A ~> do
--         putStrLn "A was pressed!"
--         setLEDs chan $ LEDState (True, False, False, False)
--         -- HID.writeOutputReport dev . B.pack $ [0x11, bit 7 .|. 0]
--         pure ()
--     Released A ~> do
--         putStrLn "A was released :("
--         setLEDs chan $ LEDState (False, True, True, False)
--         pure ()
--     Disconnected ~> do
--         putStrLn "Reconnecting..."
--         tryReconnect chan
--     Connected ~> do
--         putStrLn "Reconnected!"
--     ReconnectFailed ~> do
--         putStrLn "Reconnecting again..."
--         tryReconnect chan


