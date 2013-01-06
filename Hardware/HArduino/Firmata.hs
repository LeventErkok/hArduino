{-# LANGUAGE NamedFieldPuns #-}
module Hardware.HArduino.Firmata where

import Data.Bits
import Control.Monad (unless)
import qualified System.USB as USB

import Hardware.HArduino.Data
import Hardware.HArduino.Parts

setPinMode :: Arduino -> Pin -> Mode -> IO ()
setPinMode arduino p m = do
        let ArduinoChannel{send} = getChannel arduino
            cmd = [firmata SetPinMode .|. firmata p, firmata m]
        b <- send cmd USB.noTimeout
        unless b $ error $ "setPinMode: Failed to set pin mode " ++ show m ++ " on pin " ++ show p

digitalWrite :: Arduino -> Pin -> Bool -> IO ()
digitalWrite arduino p v = do
        let ArduinoChannel{send} = getChannel arduino
            cmd = [firmata DigitalMessage .|. firmata p, firmata v]
        b <- send cmd USB.noTimeout
        unless b $ error $ "digitalWrite: Failed to write " ++ show v ++ " on pin " ++ show p
