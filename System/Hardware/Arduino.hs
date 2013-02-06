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
  -- ** Accessing pins
  , pin, Pin, PinMode(..), setPinMode
  -- ** Digital I/O
  -- *** Writing digital values
  , digitalWrite
  -- *** Reading digital values
  , digitalRead,  pullUpResistor, waitFor, waitAny, waitAnyHigh, waitAnyLow
  -- ** Analog Communication
  -- *** Setting up sampling interval
  , setAnalogSamplingInterval
  -- *** Reading analog values
  , analogRead
  -- * Controlling LCDs
  -- ** LCD types and registration
  , LCDController(..), lcdRegister
  -- ** Writing text on the LCD
  , lcdClear, lcdWrite
  -- ** Moving the cursor
  , lcdHome, lcdSetCursor
  -- ** Scrolling
  , lcdAutoScrollOff, lcdAutoScrollOn
  , lcdScrollDisplayLeft, lcdScrollDisplayRight
  -- ** Controlling LCD display properties
  , lcdLeftToRight, lcdRightToLeft
  , lcdDisplayOff, lcdDisplayOn
  , lcdCursorOn, lcdCursorOff
  , lcdNoBlink, lcdBlink
  -- * Misc utilities
  , delay
 )
 where

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Comm
import System.Hardware.Arduino.Firmata
import System.Hardware.Arduino.LCD
