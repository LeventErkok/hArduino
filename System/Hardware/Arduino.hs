-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- (The hArduino library is hosted at <http://leventerkok.github.com/hArduino/>.
-- Comments, bug reports, and patches are always welcome.)
--
-- hArduino: Control Arduino from Haskell, using the Firmata protocol.
--
-- The hArduino library allows construction of Haskell programs that control
-- Arduino boards that are running the (freely available) Firmata program. Note
-- that hArduino does /not/ allow you to run arbitrary Haskell code on the
-- Arduino! It simply allows you to control a board from Haskell, where you
-- can exchange information with the board, send/receive commands from other
-- peripherals connected, etc.
--
-- See <http://www.youtube.com/watch?v=PPa3im44t2g> for a short video (4m29s)
-- of the blink example.
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
