-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- hArduino allows Haskell programs to control Arduino boards (<http://www.arduino.cc>)
-- and peripherals, using the Firmata protocol (<http://firmata.org>).
--
-- For details, see: <http://leventerkok.github.com/hArduino>.
-------------------------------------------------------------------------------
module System.Hardware.Arduino (
  -- * Running the controller
  withArduino, Arduino
  -- * Programming the Arduino
  -- ** Pins
  , analog, digital, pin, Pin, PinMode(..), setPinMode
  -- ** Analog input
  , analogRead
  -- ** Digital I/O
  , digitalWrite, digitalRead
  -- ** Programming with triggers
  , waitFor, waitAny, waitAnyHigh, waitAnyLow
  -- ** Receiving and sending pulses
  , pulse, pulseIn_hostTiming, pulseOut_hostTiming
  -- * Misc utilities
  , setAnalogSamplingInterval, pullUpResistor, delay, time, timeOut
  , queryFirmware
 )
 where

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Comm
import System.Hardware.Arduino.Firmata
