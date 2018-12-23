module Command where

import           Control.Concurrent           (ThreadId, killThread)
import           Control.Concurrent.MVar      (MVar, readMVar)
import           Control.Concurrent.STM       (STM, atomically)
import           Control.Concurrent.STM.TChan
import qualified Data.ByteString              as B
import           Wiimote

data WiimoteRef = WiimoteRef
                    { threadId_      :: ThreadId -- for harsh shutdowns?
                    , commandChannel :: TChan Command
                    , refState_      :: MVar WiimoteState }

refState :: WiimoteRef -> IO WiimoteState
refState = readMVar . refState_

data Command
    = LED LEDState
    | Reconnect String
    | StatusReport
    | Enumerate
    | Shutdown
    deriving (Show)

sendable :: Command -> Bool
sendable (LED _)      = True
sendable StatusReport = True
sendable _            = False

commandToPacket :: Command -> B.ByteString
commandToPacket (LED ledState) = B.pack $ [0x11, ledByte ledState]
commandToPacket StatusReport   = B.pack $ [0x15, 0x00]


-- Set the LEDs of a wiimote
setLEDs :: WiimoteRef -> LEDState -> IO ()
setLEDs ref led = do
    x <- atomically $ writeTChan (commandChannel ref) (LED led)
    x `seq` pure ()

tryReconnect :: WiimoteRef -> String -> IO ()
tryReconnect ref serial = atomically $ writeTChan (commandChannel ref) (Reconnect serial)

-- disconnect :: WiimoteRef -> IO ()
-- disconnect ref =

enumerateWiimotes :: WiimoteRef -> IO ()
enumerateWiimotes ref = atomically $ writeTChan (commandChannel ref) Enumerate

forceShutdown :: WiimoteRef -> IO ()
forceShutdown = killThread . threadId_
