module Parser where

import           Control.Monad              (replicateM)
import           Data.Attoparsec.ByteString
import           Data.Bits
import           Wiimote
import Data.ByteString (ByteString)

parseReport :: ByteString -> Maybe WiimoteState 
parseReport = maybeResult . parse parseReport_ 

parseReport_ :: Parser WiimoteState
parseReport_ = do
    reportType <- anyWord8
    case reportType of
        0x20 -> do -- status
            buttons <- parseButtons
            statusFlags <- parseStatusFlags
            replicateM 2 anyWord8 -- skip 2 bytes
            batteryLevel <- fromIntegral <$> anyWord8 :: Parser Int
            pure $ defaultWiimote
                { buttons   = buttons
                , leds      = Just $ leds_ statusFlags
                , lowBattery = Just $ lowBattery_ statusFlags
                , extensionConnected = Just $ extensionConnected_ statusFlags }
        0x21 -> undefined -- Read memory
        0x22 -> do -- handling errors
            bytes <- replicateM 4 anyWord8
            error $ "Error with data: " ++ show bytes
        0x30 -> do
            buttons <- parseButtons
            pure $ defaultWiimote { buttons = buttons }
        0x31 -> do
            buttons <- parseButtons
            accData <- parseAccData
            pure $ defaultWiimote
                { buttons       = buttons
                , accelerometer = Just accData }



data StatusFlags = StatusFlags
            { lowBattery_         :: Bool
            , extensionConnected_ :: Bool
            , speakerEnabled      :: Bool
            , irEnabled           :: Bool
            , leds_               :: LEDState } deriving (Show)

parseStatusFlags :: Parser StatusFlags
parseStatusFlags = do
    flags <- anyWord8
    pure $ StatusFlags
        { lowBattery_ = testBit flags 0
        , extensionConnected_ = testBit flags 1
        , speakerEnabled = testBit flags 2
        , irEnabled = testBit flags 3
        , leds_ = LEDState (testBit flags 4, testBit flags 5, testBit flags 6, testBit flags 7)}

parseAccData :: Parser AccState
parseAccData = undefined

-- See http://wiibrew.org/wiki/Wiimote#Buttons
-- for a description of this format
parseButtons :: Parser ButtonState
parseButtons = do
    byte1 <- anyWord8
    byte2 <- anyWord8
    pure $ ButtonState
        { a         = testBit byte2 3
        , b         = testBit byte2 2
        , plus      = testBit byte1 4
        , minus     = testBit byte2 4
        , home      = testBit byte2 7
        , one       = testBit byte2 1
        , two       = testBit byte2 0
        , dpadLeft  = testBit byte1 0
        , dpadRight = testBit byte1 1
        , dpadDown  = testBit byte1 2
        , dpadUp    = testBit byte1 3 }

