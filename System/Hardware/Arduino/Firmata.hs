{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata where

import Data.Bits

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Parts

setPinMode :: Arduino -> Pin -> Mode -> IO ()
setPinMode arduino p m = do
        let ArduinoChannel{send} = getChannel arduino
        send [firmata SetPinMode .|. firmata p, firmata m]

digitalWrite :: Arduino -> Pin -> Bool -> IO ()
digitalWrite arduino p v = do
        let ArduinoChannel{send} = getChannel arduino
        send [firmata DigitalMessage .|. firmata p, firmata v]
