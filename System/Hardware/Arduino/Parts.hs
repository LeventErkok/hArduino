-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Firmata.Parts
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Basic Arduino hardware abstractions, pins etc.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.Parts(PinMode(..), Pin, pin, pinNo, pinPort) where

-- | A pin on the Arduino
data Pin = Pin { pinNo :: Int   -- ^ The pin number
               }
         deriving Eq

instance Show Pin where
  show p | i < 10 = "Pin0" ++ show i
         | True   = "Pin"  ++ show i
   where i = pinNo p

-- | Smart constructor for a pin. The input should be between 1 and 13:
--
--     * Pins 0-1 are reserved for TX/RX; so can't be directly used.
--
--     * 13 pins is UNO specific, we will need to expand this definition if
--       we start supporting other boards.
pin :: Int -> Pin
pin i | i < 2 || i > 13 = error $ "Invalid pin number: " ++ show i
      | True            = Pin i

-- | On the Arduino, pins are grouped into banks of 8.
-- Given a pin, this function determines which port/index it belongs to
pinPort :: Pin -> (Int, Int)
pinPort p = pinNo p `quotRem` 8

-- | The mode for a pin.
data PinMode = INPUT
             | OUTPUT
             | ANALOG
             | PWM
             | SERVO
             deriving (Eq, Show, Enum)
