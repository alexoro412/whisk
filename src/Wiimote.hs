module Wiimote where

data WiimoteState = WiimoteState 
    { buttons :: ButtonState 
    , nunchuk :: Maybe NunchukState 
    , accelerometer :: Maybe AccState 
    , ir :: Maybe IRState 
    , extensionConnected :: Maybe Bool 
    , rumble :: Bool 
    , leds :: Maybe LEDState 
    , lowBattery :: Maybe Bool } deriving (Show)

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

data ButtonState = ButtonState 
    { a :: Bool 
    , b :: Bool 
    , one :: Bool 
    , two :: Bool 
    , minus :: Bool 
    , plus :: Bool 
    , home :: Bool 
    , dpadLeft :: Bool 
    , dpadRight :: Bool 
    , dpadDown :: Bool 
    , dpadUp :: Bool } deriving (Show)

defaultButtons = ButtonState 
    { a = False, b = False, one = False, two = False 
    , minus = False, plus = False, home = False 
    , dpadLeft = False, dpadRight = False 
    , dpadUp = False, dpadDown = False }

data NunchukState = NunchukState 
    { btnX :: Double 
    , btnY :: Double 
    , c :: Bool 
    , z :: Bool } deriving (Show) -- TODO add nunchuk accelerometer 

data AccState = AccState 
    { accX :: Double 
    , accY :: Double 
    , accZ :: Double } deriving (Show)

data ExtensionType = Nunchuk deriving (Show) -- TODO support more extensions

data IRState = NotImplemented deriving (Show)
    -- TODO finish this one 