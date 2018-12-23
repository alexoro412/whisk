{-# LANGUAGE LambdaCase #-}

module Wiimote where

import           Data.Bits       (bit, (.|.))
import           Data.Word       (Word8)

data WiimoteState = WiimoteState
    { buttons            :: ButtonState
    , nunchuk            :: Maybe NunchukState
    , accelerometer      :: Maybe AccState
    , ir                 :: Maybe IRState
    , extensionConnected :: Maybe Bool
    , rumble             :: Bool
    , leds               :: Maybe LEDState
    , lowBattery         :: Maybe Bool } deriving (Show)

defaultWiimote = WiimoteState
    { buttons = defaultButtons
    , nunchuk = Nothing
    , accelerometer = Nothing
    , ir = Nothing
    , extensionConnected = Nothing
    , rumble = False
    , leds = Nothing
    , lowBattery = Nothing }

data LEDState = LEDState (Bool, Bool, Bool, Bool) deriving (Show)

ledByte :: LEDState -> Word8
ledByte (LEDState (led1, led2, led3, led4)) = foldr (.|.) 0 $ zipWith (\index lit -> if lit then bit index else 0) [4..7] [led1, led2, led3, led4]

data ButtonState = ButtonState
    { a         :: Bool
    , b         :: Bool
    , one       :: Bool
    , two       :: Bool
    , minus     :: Bool
    , plus      :: Bool
    , home      :: Bool
    , dpadLeft  :: Bool
    , dpadRight :: Bool
    , dpadDown  :: Bool
    , dpadUp    :: Bool } deriving (Show)

defaultButtons = ButtonState
    { a = False, b = False, one = False, two = False
    , minus = False, plus = False, home = False
    , dpadLeft = False, dpadRight = False
    , dpadUp = False, dpadDown = False }

data NunchukState = NunchukState
    { joyX :: Double
    , joyY :: Double
    , c    :: Bool
    , z    :: Bool } deriving (Show) -- TODO add nunchuk accelerometer

data AccState = AccState
    { accX :: Double
    , accY :: Double
    , accZ :: Double } deriving (Show)

data ExtensionType = Nunchuk deriving (Show) -- TODO support more extensions

data IRState = NotImplemented deriving (Show)
    -- TODO finish this one

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

isDirectional :: Button -> Bool 
isDirectional = \case 
    DpadDown -> True 
    DpadUp -> True 
    DpadLeft -> True 
    DpadRight -> True 
    _ -> False 
