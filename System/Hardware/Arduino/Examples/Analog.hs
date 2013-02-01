-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Examples.Analog
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Reads the value of an analog input, controlled by a 10K potentiometer.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.Examples.Analog where

import Control.Monad       (when)
import Control.Monad.Trans (liftIO)

import System.Hardware.Arduino

-- | Read the value of an analog input line. The circuit simply
-- has a 10K potentiometer between 5V and GND, with the wiper
-- line connected to analog input 0.
analogVal :: IO ()
analogVal = withArduino False "/dev/cu.usbmodemfd131" $ do
               setPinMode pot ANALOG
               cur <- analogRead pot
               liftIO $ print cur
               go cur
  where pot = pin 14 -- NB. Analog-0 is pin 14 on the UNO
        go prev = do delay 100
                     new <- analogRead pot
                     when (prev /= new) $ liftIO $ print new
                     go new
