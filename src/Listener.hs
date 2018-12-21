module Listener where

import           Control.Concurrent           (ThreadId, forkFinally, forkIO,
                                               threadDelay)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad                (mapM_)
import           Control.Monad.State
import qualified Data.ByteString              as B
import           Data.IORef                   (modifyIORef', newIORef,
                                               readIORef)
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Word
import           Parser
import qualified System.HID                   as HID
import           Wiimote
import Data.Bits (bit, (.|.))

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

type EventState = Map WiimoteEvent (IO ())

type EventBuilder = State EventState ()

data WiimoteEvent
    = Pressed Button
    | Released Button
    | Disconnected 
    | Connected 
    | ReconnectFailed
    deriving (Show, Eq, Ord)

infixr 0 ~>
(~>) :: WiimoteEvent -> IO () -> EventBuilder
event ~> action = modify $ M.insert event action

wiimoteVendorId, wiimoteProductId :: Word16
wiimoteVendorId = 1406
wiimoteProductId = 774

-- 00-1f-32-98-96-4b

getWiimote :: String -> IO (Maybe HID.Device)
getWiimote serial = HID.vendorProductSerialDevice wiimoteVendorId wiimoteProductId (Just serial)

listWiimotes :: IO [HID.DeviceInfo]
listWiimotes = devInfos where
    devInfos = HID.enumerate (fromIntegral wiimoteVendorId) (fromIntegral wiimoteProductId)

listWiimoteSerials :: IO [String]
listWiimoteSerials = map HID.devSerialNumber <$> listWiimotes

withWiimote :: String -> (TChan Command -> EventBuilder) -> IO ()
withWiimote serial builderf = do
    mdevice <- getWiimote serial
    case mdevice of
        Nothing -> error "Wiimote not found" -- todo change to trigger event?
        Just device -> do
            (t, chan, cmdChan) <- startLoop device
            let builder = builderf cmdChan 
                eventMap = execState builder M.empty
                loop = do
                    event <- atomically $ readTChan chan
                    -- putStrLn $ "EVENT: " ++ show event
                    case M.lookup event eventMap of
                        Nothing     -> pure ()
                        Just action -> action
                    loop
            loop
    -- mdevice <- HID.vendorProductSerialDevice wiimoteVendorId wiimoteProductId (Just serial)
    -- case mdevice of
    --     Nothing -> error "Wiimote not found" -- todo replace this
    --     Just device -> do
    --         mReport <- HID.readInputReport device
    --         case mReport of
    --             Nothing -> error "Why does this happen"
    --             Just b -> do
    --                 let Just report = parseReport b
    --                 print report
    --                 if (a . buttons) report
    --                     then print "A was pressed"
    --                     else print "A was not pressed"

type WiimoteLoop = (ThreadId, TChan WiimoteEvent, TChan Command)

data Command = LED LEDState | Reconnect 

setLEDs :: TChan Command -> LEDState -> IO () 
setLEDs chan led = do 
    x <- atomically $ writeTChan chan (LED led)
    x `seq` pure ()

wiimoteButtons = zip
    [A, B, One, Two, Minus, Plus, Home, DpadLeft, DpadRight, DpadDown, DpadUp]
    $ map (. buttons) [a, b, one, two, minus, plus, home, dpadLeft, dpadRight, dpadDown, dpadUp]

-- first WiimoteState is the new one
diffWiimote :: WiimoteState -> WiimoteState -> [WiimoteEvent]
diffWiimote wa wb = 
    [ if f wa then Pressed b else Released b | (b,f) <- wiimoteButtons, f wa /= f wb]

tryReconnect :: TChan Command -> IO () 
tryReconnect chan = atomically $ writeTChan chan Reconnect

startLoop :: HID.Device -> IO WiimoteLoop
startLoop dev_ = do
    state <- newIORef defaultWiimote
    chan <- newTChanIO :: IO (TChan WiimoteEvent)
    cmdChan <- newTChanIO :: IO (TChan Command)
    HID.setBlocking dev_ False
    serial <- HID.getSerialNumber dev_
    timer <- newIORef 0 
    let loop dev = do
                modifyIORef' timer (+1)
                t <- readIORef timer 
                if (t >= 20) 
                    then do  
                        modifyIORef' timer (const 0)
                        sendPacket dev [0x15, 0x00]
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
                                let packet = case cmd of 
                                        LED (LEDState (led1, led2, led3, led4)) ->
                                            [0x11, foldr (.|.) 0 $ zipWith (\x y -> if y then bit x else 0) [4..7] [led1, led2,led3,led4]] 
                                sendPacket dev packet 
        sendPacket dev pkt = do 
            sendPacketAndThen dev pkt (threadDelay 100 >> loop dev) (atomically (writeTChan chan Disconnected) >> waitLoop)
        waitLoop = do 
            threadDelay $ 10^6
            cmd <- atomically $ readTChan cmdChan 
            case cmd of 
                Reconnect -> do 
                    mdevice <- getWiimote serial 
                    case mdevice of 
                        Nothing -> do 
                            atomically $ writeTChan chan ReconnectFailed
                            waitLoop  
                        Just dev -> do 
                            atomically $ writeTChan chan Connected 
                            HID.setBlocking dev False
                            loop dev 


    t <- forkIO $ loop dev_
    pure $ (t, chan, cmdChan)

sendPacketAndThen :: HID.Device -> [Word8] -> IO a -> IO b -> IO ()
sendPacketAndThen dev packet success failure = do 
    bytesWritten <- HID.writeOutputReport dev $ B.pack packet 
    if bytesWritten < length packet 
        then void failure 
        else void success
