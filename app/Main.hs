module Main where

import           Control.Concurrent           (killThread, threadDelay, forkIO)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan)
import           Control.Monad                (when)
import           Data.Bits                    ((.|.), bit)
import qualified Data.ByteString              as B
import           Data.Traversable             (for)
import           Listener
import qualified System.HID                   as HID
import           Text.Read                    (readMaybe)
import           Wiimote
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket_)

main :: IO ()
main = do
    HID.init
    serials <- listWiimoteSerials
    case serials of
        [] -> putStrLn "No wiimotes found"
        serials -> do
            let options = zip [1..] serials
            for options $ \(num, serial) -> do
                putStr "("
                putStr . show $ num
                putStr ") "
                putStrLn serial
            option <- getNumberInRange (1, length options)
            let Just serial = lookup option options
            shutdown <- newEmptyMVar 
            forkIO $ bracket_ 
                (pure ())
                (putMVar shutdown True)
                (withWiimote serial eventHandler)
            shutdown <- takeMVar shutdown
            pure ()
            

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

eventHandler chan = do
    Pressed A ~> do
        putStrLn "A was pressed!"
        setLEDs chan $ LEDState (True, False, False, False)
        -- HID.writeOutputReport dev . B.pack $ [0x11, bit 7 .|. 0]
        pure ()
    Released A ~> do
        putStrLn "A was released :("
        setLEDs chan $ LEDState (False, True, True, False)
        pure ()
    Disconnected ~> do 
        putStrLn "Reconnecting..."
        tryReconnect chan
    Connected ~> do 
        putStrLn "Reconnected!"
    ReconnectFailed ~> do 
        putStrLn "Reconnecting again..."
        tryReconnect chan 


