{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata where

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Parts
import System.Hardware.Arduino.Protocol

queryFirmware :: Arduino -> IO Response
queryFirmware arduino = do
        let ArduinoChannel{send, recv} = getChannel arduino
        send QueryFirmware
        recv

setPinMode :: Arduino -> Pin -> Mode -> IO ()
setPinMode arduino p m = do
        let ArduinoChannel{send} = getChannel arduino
        send $ SetPinMode p m

digitalWrite :: Arduino -> Pin -> Bool -> IO ()
digitalWrite arduino p v = do
        let ArduinoChannel{send} = getChannel arduino
        send $ DigitalWrite p v
