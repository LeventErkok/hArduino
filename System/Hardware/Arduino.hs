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
-------------------------------------------------------------------------------
module System.Hardware.Arduino (
  -- * Running the controller
  withArduino, Arduino
  -- * Programming the Arduino
  -- ** Basic handshake with the board
  , queryFirmware
  -- ** Controlling the pins
  , setPinMode
  -- ** Reading and Writing digital values
  -- , digitalRead, digitalWrite
  -- ** Misc utilities
  , delay
  -- * Hardware components on the board
  -- ** Pins
  , pin, PinMode(..)
 )
 where

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Comm
import System.Hardware.Arduino.Firmata
