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

module System.Hardware.Arduino.Parts where

import Data.Word

data Pin = Pin { pinVal :: Word8 }
         deriving Eq

pinPort :: Pin -> (Int, Int)
pinPort p = fromIntegral (pinVal p) `quotRem` 8

instance Show Pin where
  show (Pin i) | i < 10 = "Pin0" ++ show i
               | True   = "Pin"  ++ show i

-- 13 pins is UNO specific, should really have the board here
pin :: Int -> Pin
pin i | i < 2 || i > 13 = error $ "Invalid pin number: " ++ show i
      | True            = Pin (fromIntegral i)

data Mode = INPUT
          | OUTPUT
          | ANALOG
          | PWM
          | SERVO
          deriving (Show, Enum)
