module Listener where

import           Control.Concurrent           (ThreadId, forkFinally, forkIO,
                                               threadDelay)
import           Control.Concurrent.STM       (atomically, STM)
import           Control.Concurrent.STM.TChan
import           Control.Monad                (mapM_)
import           Control.Monad.State
import           Data.Bits                    (bit, (.|.))
import qualified Data.ByteString              as B
import           Data.IORef                   (IORef, modifyIORef', newIORef,
                                               readIORef)
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Word
import           Parser
import qualified System.HID                   as HID
import           Wiimote
import Control.Exception (onException)

data Button
    = A
    | B
    | One
    | Two
    | Plus
    | Minus
    | Home
    | DpadLeft
    | DpadRight
    | DpadDown
    | DpadUp
    deriving (Show, Eq, Ord)

-- type EventState = Map WiimoteEvent (IO ())

-- type EventBuilder = State EventState ()

data WiimoteEvent
    = Pressed Button
    | Released Button
    | Disconnected
    | Connected String 
    | ReconnectFailed
    | Enumeration [String]
    deriving (Show, Eq, Ord)

data Command
    = LED LEDState
    | Reconnect String 
    | StatusReport
    | Enumerate 

data WiimoteRef = WiimoteRef
                    { threadId_      :: ThreadId -- for harsh shutdowns?
                    , commandChannel :: TChan Command }

type WiimoteLoop = (ThreadId, TChan WiimoteEvent, TChan Command)

-- Constants used for finding wiimotes over HID
wiimoteVendorId, wiimoteProductId :: Word16
wiimoteVendorId = 1406
wiimoteProductId = 774

-- Open a wiimote using its serial number 
getWiimote :: String -> IO (Maybe HID.Device)
getWiimote serial = HID.vendorProductSerialDevice wiimoteVendorId wiimoteProductId (Just serial)

-- List all connected wiimotes.
listWiimotes :: IO [HID.DeviceInfo]
listWiimotes = devInfos where
    devInfos = HID.enumerate (fromIntegral wiimoteVendorId) (fromIntegral wiimoteProductId)

-- Just list wiimote serial numbers
listWiimoteSerials :: IO [String]
listWiimoteSerials = map HID.devSerialNumber <$> listWiimotes

-- With a wiimote, and an event handler, start a loop and handle events.
withWiimote :: String -> (WiimoteRef -> WiimoteEvent -> IO ()) -> IO ()
withWiimote serial handler = do
    mdevice <- getWiimote serial
    case mdevice of
        Nothing -> error "Wiimote not found" -- todo change to trigger event?
        Just device -> do
            (t, chan, cmdChan) <- startLoop device
            let wiiRef = WiimoteRef 
                            { threadId_ = t 
                            , commandChannel = cmdChan }
            handler wiiRef (Connected serial) -- send a first connected event
            let loop = do
                    (atomically $ readTChan chan) >>= handler wiiRef
                    loop
            loop 

wiimoteButtons = zip
    [A, B, One, Two, Minus, Plus, Home, DpadLeft, DpadRight, DpadDown, DpadUp]
    $ map (. buttons) [a, b, one, two, minus, plus, home, dpadLeft, dpadRight, dpadDown, dpadUp]

-- first WiimoteState is the new one
diffWiimote :: WiimoteState -> WiimoteState -> [WiimoteEvent]
diffWiimote wa wb =
    [ if f wa then Pressed b else Released b | (b,f) <- wiimoteButtons, f wa /= f wb]



startLoop :: HID.Device -> IO WiimoteLoop
startLoop dev_ = do
    -- Stores the wiimotes current state 
    -- TODO, allow the user to access this directly?
    state <- newIORef defaultWiimote

    -- Channel for sending events 
    chan <- newTChanIO :: IO (TChan WiimoteEvent)

    -- Channel for receiving commands 
    cmdChan <- newTChanIO :: IO (TChan Command)

    -- Disable blocking
    HID.setBlocking dev_ False

    -- A timer for send status requests
    timer <- newIORef 0 :: IO (IORef Int)

    let loop dev = do
                modifyIORef' timer (+1)
                t <- readIORef timer
                if (t >= 20) -- TODO replace magic number
                    then do
                        modifyIORef' timer (const 0) -- reset timer
                        sendCommand dev StatusReport -- ask for status report
                    else do
                        mReport <- HID.readInputReport dev
                        case mReport of
                            Just report -> do
                                case parseReport report of
                                    Nothing -> pure ()
                                    Just newState  -> do
                                        oldState <- readIORef state
                                        let events = diffWiimote newState oldState
                                        modifyIORef' state $ const newState
                                        atomically $ mapM_ (writeTChan chan) events
                                        -- atomically $ writeTChan chan r
                            Nothing     -> pure ()
                        -- read from channel here
                        mcmd <- atomically $ tryReadTChan cmdChan
                        mcmd `seq` case mcmd of
                            Nothing -> do
                                threadDelay 100
                                loop dev
                            Just cmd -> do
                                -- TODO are there some commands that can't be sent?
                                -- Yes, reconnect and enumerate...
                                sendCommand dev cmd
        sendCommand dev cmd = do
            sendPacketAndThen dev (commandToPacket cmd)
                -- On successfully sending a command, resume loop
                (threadDelay 100 >> loop dev) 
                -- When a wiimote is disconnected, let the user know
                (atomically (writeTChan chan Disconnected) >> waitLoop) 
        waitLoop = do
            -- TODO better exception handling!!!!
            cmd <- (atomically $ readTChan cmdChan) `onException` (error "Blocked")
            case cmd of
                Reconnect s -> do
                    mdevice <- getWiimote s
                    case mdevice of
                        Nothing -> do
                            atomically $ writeTChan chan ReconnectFailed
                            waitLoop
                        Just dev -> do
                            atomically $ writeTChan chan (Connected s)
                            HID.setBlocking dev False
                            loop dev
                Enumerate -> do 
                    serials <- listWiimoteSerials
                    atomically $ writeTChan chan (Enumeration serials)
                    waitLoop 
                _ -> waitLoop -- ignore other commands when in reconnect mode
    -- TODO how to handle the owner thread crashing 
    -- but the loop keeps running? Some sort of link?
    t <- forkIO $ loop dev_
    pure $ (t, chan, cmdChan)

commandToPacket :: Command -> B.ByteString
commandToPacket (LED ledState) = B.pack $ [0x11, ledByte ledState]
commandToPacket StatusReport   = B.pack $ [0x15, 0x00]

sendPacketAndThen :: HID.Device -> B.ByteString -> IO a -> IO b -> IO ()
sendPacketAndThen dev packet success failure = do
    bytesWritten <- HID.writeOutputReport dev packet
    if bytesWritten < B.length packet
        then void failure
        else void success

-- Wiimote Commands 

-- Set the LEDs of a wiimote
setLEDs :: WiimoteRef -> LEDState -> IO ()
setLEDs ref led = do
    x <- atomically $ writeTChan (commandChannel ref) (LED led)
    x `seq` pure ()

tryReconnect :: WiimoteRef -> String -> IO ()
tryReconnect ref serial = atomically $ writeTChan (commandChannel ref) (Reconnect serial)

emptyTChan :: TChan a -> STM ()
emptyTChan chan = do
            empty <- isEmptyTChan chan 
            if empty
                then pure ()
                else do
                    _ <- readTChan chan
                    emptyTChan chan

enumerateWiimotes :: WiimoteRef -> IO ()
enumerateWiimotes ref = atomically $ writeTChan (commandChannel ref) Enumerate 