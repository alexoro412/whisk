{-# LANGUAGE LambdaCase #-}

module Listener where

import           Command
import           Control.Concurrent           (ThreadId, forkFinally, forkIO,
                                               killThread, myThreadId,
                                               threadDelay)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Exception            (BlockedIndefinitelyOnSTM,
                                               Exception, SomeException, catch,
                                               displayException, onException,
                                               throwIO, throwTo, toException)
import           Control.Monad                (mapM_)
import           Control.Monad.State
import qualified Data.ByteString              as B
import           Data.IORef                   (IORef, modifyIORef', newIORef,
                                               readIORef)
import           Data.Word
import           Parser
import qualified System.HID                   as HID
import           Wiimote

import Control.Concurrent.MVar 

data WiimoteException
    = HarshShutdown
    | WiimoteNotFound
    deriving (Show)

instance Exception WiimoteException

data WiimoteEvent
    = Pressed Button
    | Released Button
    | Disconnected
    | Connected String
    | ReconnectFailed
    | Enumeration [String] 
    deriving (Show)

type WiimoteLoop = (ThreadId, TChan WiimoteEvent, TChan Command, MVar WiimoteState)

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
    getWiimote serial >>= \case
        Nothing -> throwIO WiimoteNotFound
        Just device -> do
            (t, chan, cmdChan, wiimoteState) <- startLoop device
            let wiiRef = WiimoteRef
                            { threadId_ = t
                            , commandChannel = cmdChan
                            , refState_ = wiimoteState }
            handler wiiRef (Connected serial) -- send a first connected event
            let loop = do
                    event <- (atomically $ readTChan chan) `catch` (\e -> do
                                                                        let err = (e :: BlockedIndefinitelyOnSTM)
                                                                        throwIO HarshShutdown)
                    x <- (handler wiiRef event) 
                    x `seq` loop
            loop

wiimoteButtons :: [(Button, WiimoteState -> Bool)]
wiimoteButtons = zip
    [A, B, One, Two, Minus, Plus, Home, DpadLeft, DpadRight, DpadDown, DpadUp]
    $ map (. buttons) [a, b, one, two, minus, plus, home, dpadLeft, dpadRight, dpadDown, dpadUp]

-- Lists the differences between two wiimote states
-- first WiimoteState is the new one
diffWiimote :: WiimoteState -> WiimoteState -> [WiimoteEvent]
diffWiimote wa wb =
    [ if f wa then Pressed b else Released b | (b,f) <- wiimoteButtons, f wa /= f wb]


startLoop :: HID.Device -> IO WiimoteLoop
startLoop dev_ = do
    -- Stores the wiimotes current state
    -- TODO, allow the user to access this directly?
    state <- newMVar defaultWiimote

    -- Channel for sending events
    chan <- newTChanIO :: IO (TChan WiimoteEvent)

    -- Channel for receiving commands
    cmdChan <- newTChanIO :: IO (TChan Command)

    -- Disable blocking
    HID.setBlocking dev_ False

    -- A timer for send status requests
    timer <- newIORef 0 :: IO (IORef Int)

    ownerThread <- myThreadId

    let loop dev = do
            -- Uses a mutable timer to regularly 
            -- send the status request
            modifyIORef' timer (+1)
            t <- readIORef timer

            stillConnected <- if (t >= 20) 
                then do
                    modifyIORef' timer (const 0) -- reset timer
                    sendCommand dev StatusReport -- ask for status report
                else pure True 

            if not stillConnected 
                then do 
                    onDisconnection
                else do 
                    -- Check for a report
                    mReport <- HID.readInputReport dev
                    case mReport >>= parseReport of
                        Just newState -> do
                            -- When there is a report, see what's changed,
                            -- and first the event handler.
                            oldState <- readMVar state
                            let events = diffWiimote newState oldState
                            swapMVar state newState
                            atomically $ mapM_ (writeTChan chan) events
                        Nothing     -> pure ()

                    mcmd <- atomically $ tryReadTChan cmdChan
                    mcmd `seq` case mcmd of
                        Nothing -> do
                            threadDelay 50000
                            loop dev
                        Just cmd
                            | sendable cmd -> do
                                sendCommand dev cmd >>= \case 
                                    True -> loop dev 
                                    False -> do 
                                        onDisconnection
                                        -- waitLoop 
                            | otherwise -> loop dev -- Ignores some commands when connected 
        
        onDisconnection = do 
            atomically $ writeTChan chan Disconnected 
            waitLoop
        waitLoop = do
            -- This read fails when the parent thread loses 
            -- its reference to the command channel. In that case
            -- the event handler is down as well, so this should 
            -- shutdown 
            cmd <- catch 
                    (atomically $ readTChan cmdChan) 
                    (const (pure Shutdown) :: BlockedIndefinitelyOnSTM -> IO Command)
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
                Shutdown -> do
                    pure ()
                _ -> waitLoop -- ignore other commands when in reconnect mode
    -- TODO how to handle the owner thread crashing
    -- but the loop keeps running? Some sort of link?
    -- Will this ever be a problem? It doesn't seem to be
    t <- forkFinally (loop dev_) $ \case
                    Left e -> do
                        print e
                        throwTo ownerThread e
                    _ -> pure ()

    pure $ (t, chan, cmdChan, state)

-- Sends a command
-- Returns a boolean to say whether the data was sent 
-- successfully 
sendCommand :: HID.Device -> Command -> IO Bool
sendCommand dev command = do
    let packet = commandToPacket command
    bytesWritten <- HID.writeOutputReport dev packet
    pure $ bytesWritten == B.length packet

