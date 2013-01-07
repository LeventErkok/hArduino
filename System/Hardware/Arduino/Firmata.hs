{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata where

import Data.Word

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Parts
import System.Hardware.Arduino.Protocol

queryFirmware :: Arduino -> IO (Word8, Word8, String)
queryFirmware arduino = do
        let ArduinoChannel{send, recv} = getChannel arduino
        send QueryFirmware
        r <- recv
        case r of
          Firmware v1 v2 m -> return (v1, v2, m)
          _                -> error $ "Got unexpected response for query firmware call: " ++ show r

setPinMode :: Arduino -> Pin -> Mode -> IO ()
setPinMode arduino p m = do
        let ArduinoChannel{send} = getChannel arduino
        send $ SetPinMode p m

digitalRead :: Arduino -> Pin -> IO Bool
digitalRead arduino p = do
        let ArduinoChannel{send, recv} = getChannel arduino
        send $ DigitalRead p
        r <- recv
        case r of
          PinValue b -> return b
          _          -> error $ "Got unexpected response for query digitalRead: " ++ show r

digitalWrite :: Arduino -> Pin -> Bool -> IO ()
digitalWrite arduino p v = do
        let ArduinoChannel{send} = getChannel arduino
        send $ DigitalWrite p v
