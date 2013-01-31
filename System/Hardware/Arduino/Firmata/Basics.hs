-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Firmata.Basics
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Basic components of the firmata protocol
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata.Basics where

import Control.Concurrent  (newEmptyMVar, readMVar)
import Control.Monad       (when)
import Control.Monad.Trans (liftIO)
import Data.Word           (Word8)

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Comm
import qualified System.Hardware.Arduino.Utils as U

-- | Retrieve the Firmata firmware version running on the Arduino. The first
-- component is the major, second is the minor. The final value is a human
-- readable identifier for the particular board.
queryFirmware :: Arduino (Word8, Word8, String)
queryFirmware = do
        send QueryFirmware
        r <- recv
        case r of
          Firmware v1 v2 m -> return (v1, v2, m)
          _                -> error $ "queryFirmware: Got unexpected response for query firmware call: " ++ show r

-- | Delay the computaton for a given number of milli-seconds
delay :: Int -> Arduino ()
delay = liftIO . U.delay

-- | Set the mode on a particular pin on the board
setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p m = do
   extras <- registerPinMode p m
   send $ SetPinMode p m
   mapM_ send extras

-- | Set or clear a digital pin on the board
digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p v = do
   -- first make sure we have this pin set as output
   pd <- getPinData p
   when (pinMode pd /= OUTPUT) $ U.die ("Invalid digitalWrite call on pin " ++ show p)
                                       [ "The current mode for this pin is: " ++ show (pinMode pd)
                                       , "For digitalWrite, it must be set to: " ++ show OUTPUT
                                       , "via a proper call to setPinMode"
                                       ]
   (lsb, msb) <- computePortData p v
   send $ DigitalPortWrite (pinPort p) lsb msb

-- | Read the value of a pin in digital mode; this is a non-blocking call, returning
-- the current value immediately. See 'waitFor' for a version that waits for a change
-- in the pin first.
digitalRead :: Pin -> Arduino Bool
digitalRead p = do
   -- first make sure we have this pin set as input
   pd <- getPinData p
   when (pinMode pd /= INPUT) $ U.die ("Invalid digitalRead call on pin " ++ show p)
                                      [ "The current mode for this pin is: " ++ show (pinMode pd)
                                      , "For digitalWrite, it must be set to: " ++ show INPUT
                                      , "via a proper call to setPinMode"
                                      ]
   return $ case pinValue pd of
              Just (Left v) -> v
              _             -> False -- no (correctly-typed) value reported yet, default to False

-- | Wait for a change in the value of the digital input pin. Returns the new value.
-- Note that this is a blocking call. For a non-blocking version, see 'digitalRead', which returns the current
-- value of a pin immediately.
waitFor :: Pin -> Arduino Bool
waitFor p = do
   curVal <- digitalRead p
   semaphore <- liftIO newEmptyMVar
   let wait = do digitalWakeUp semaphore
                 liftIO $ readMVar semaphore
                 newVal <- digitalRead p
                 if newVal == curVal
                    then wait
                    else return newVal
   wait
