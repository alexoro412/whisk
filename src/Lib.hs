module Lib
    ( someFunc
    ) where

import qualified System.HIDAPI as HID 

someFunc :: IO ()
someFunc = putStrLn "someFunc"
