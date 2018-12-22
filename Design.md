# Design

Misc: Enable BinaryLiterals syntax extension. 

## Wrapper around HIDAPI 

- When opening an HID connection, wrap the handler in bracket, for closing the connection. 
- Also use onException to catch an exception from HIDAPI, and send a message to the owner thread.

## Event Listener

- Listens for events like "A pressed" "nunchuk connected" or "connection lost" from the hid wrapper
- Desire for syntax something like:
```haskell 
withWiimote $ \wiimoteRef -> do 
    Pressed A ~> do 
        putStrLn "A was pressed"
        setRumble wiimoteRef True 
    Released A ~> do 
        putStrLn "A was released"
        setRumble wiimoteRef False 
```

### Events

Current events

- Disconnected
    - Need a way to detect disconnection other than a write failing. Possibly have the device regularly poll the status of the controller, and then disconnection will be detected then. Use a timer in the main loop. 
- Connected 
- ReconnectFailed 
    - TODO allow the user to say certain wiimote serials are not options. i.e. they are being used for a different player. 
- Pressed Button
- Released Button

Planned Events

- Accelerometer x y z
- Button C and Button Z (from Nunchuk)
- Extension Connected/Disconnected (only support Nunchuk for simplicity)
- Nunchuk Accelerometer
- Low Battery 

### Commands 

*TODO* can the chan be omitted in these somehow? Perhaps a ReaderT on the event monad. But then we'd need liftIO...
*TODO* Properly set rumble bit according to state

Current commands 

- tryReconnect 
- setLEDs

Planned commands 

- setRumble 
- setReportMode (delay this one, automatically set mode when nunchuk connected)
- disconnect (be sure to set LEDs all on to remind the user to turn off their remote)