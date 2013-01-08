{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata where

import Data.Word
import Data.Bits

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

digitalRead :: Arduino -> Pin -> IO (Mode, Bool)
digitalRead arduino p = do
        let ArduinoChannel{send, recv} = getChannel arduino
        send $ DigitalRead p
        r <- recv
        case r of
          DigitalPinState p' m b -> if p == p'
                                    then return (m, b)
                                    else error $ "Got unexpected response for " ++ show p' ++ " instead of " ++ show p
          _                -> error $ "Got unexpected response for query DigitalRead: " ++ show r

digitalWrite :: Arduino -> Pin -> Bool -> IO ()
digitalWrite arduino p v = do
        let ArduinoChannel{send} = getChannel arduino
            (port, idx) = pinPort p
            dr cp@(Pin i)
             | i < 14   = digitalRead arduino cp
             | True     = return (OUTPUT, False)
        oldVal <- map snd `fmap` mapM dr [Pin (fromIntegral (port * 8 + i)) | i <- [0 .. 7]]
        let [b0, b1, b2, b3, b4, b5, b6, b7] = [if i == idx then v else old | (old, i) <- zip oldVal [0 ..]]
            lsb = sum [1 `shiftL` i | (True, i) <- zip [b0, b1, b2, b3, b4, b5, b6, False] [0 .. 7]]
            msb = if b7 then 1 else 0
        send $ DigitalPortWrite port lsb msb
