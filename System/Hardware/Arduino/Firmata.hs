-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Firmata
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Implementation of the firmata protocol
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata where

import Control.Concurrent  (newEmptyMVar, readMVar)
import Control.Monad       (when, unless, void)
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

-- | Turn on/off internal pull-up resistor on an input pin
pullUpResistor :: Pin -> Bool -> Arduino ()
pullUpResistor p v = do
   -- first make sure we have this pin set as input
   pd <- getPinData p
   when (pinMode pd /= INPUT) $ U.die ("Invalid turnOnPullUpResistor call on pin " ++ show p)
                                      [ "The current mode for this pin is: " ++ show (pinMode pd)
                                      , "For turnOnPullUpResistor, it must be set to: " ++ show INPUT
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
waitFor p = head `fmap` waitAny [p]

-- | Wait for a change in any of the given pins. Once a change is detected, all the new values are
-- returned. Similar to 'waitFor', but is useful when we are watching multiple digital inputs.
waitAny :: [Pin] -> Arduino [Bool]
waitAny ps = map snd `fmap` waitGeneric ps

-- | Wait for any of the given pins to go from low to high. If all of the pins are high to start
-- with, then we first wait for one of them to go low, and then wait for one of them to go back high.
-- Returns the new values.
waitAnyHigh :: [Pin] -> Arduino [Bool]
waitAnyHigh ps = do
   curVals <- mapM digitalRead ps
   when (and curVals) $ void $ waitAnyLow ps   -- all are H to start with, wait for at least one to go low
   vs <- waitGeneric ps  -- wait for some change
   if (False, True) `elem` vs
      then return $ map snd vs
      else waitAnyHigh ps

-- | Wait for any of the given pins to go from high to low. If all of the pins are low to start
-- with, then we first wait for one of them to go high, and then wait for one of them to go back low.
-- Returns the new values.
waitAnyLow :: [Pin] -> Arduino [Bool]
waitAnyLow ps = do
   curVals <- mapM digitalRead ps
   unless (or curVals) $ void $ waitAnyHigh ps   -- all are L to start with, wait for at least one to go high
   vs <- waitGeneric ps  -- wait for some change
   if (True, False) `elem` vs
      then return $ map snd vs
      else waitAnyLow ps

-- | A utility function, waits for any change on any given pin
-- and returns both old and new values. It's guaranteed that
-- at least one returned pair have differing values.
waitGeneric :: [Pin] -> Arduino [(Bool, Bool)]
waitGeneric ps = do
   curVals <- mapM digitalRead ps
   semaphore <- liftIO newEmptyMVar
   let wait = do digitalWakeUp semaphore
                 liftIO $ readMVar semaphore
                 newVals <- mapM digitalRead ps
                 if curVals == newVals
                    then wait
                    else return $ zip curVals newVals
   wait

-- | Read the value of a pin in analog mode; this is a non-blocking call.
-- Return value is @0@ if the voltage on the pin is 0V, and @1023@ if it is
-- 5V, properly scaled.
analogRead :: Pin -> Arduino Int
analogRead p = do
   -- first make sure we have this pin set as analog
   pd <- getPinData p
   when (pinMode pd /= ANALOG) $ U.die ("Invalid analogRead call on pin " ++ show p)
                                        [ "The current mode for this pin is: " ++ show (pinMode pd)
                                        , "For analogRead, it must be set to: " ++ show ANALOG
                                        , "via a proper call to setPinMode"
                                        ]
   return $ case pinValue pd of
              Just (Right v) -> v
              _              -> 0 -- no (correctly-typed) value reported yet, default to False
