module System.Hardware.Arduino.Parts where

import System.Hardware.Arduino.Data

data Pin = Pin Int
         deriving Show

instance Firmata Pin where
  firmata (Pin i) = fromIntegral i

-- 13 pins is UNO specific, should really have the board here
pin :: Int -> Pin
pin i | i < 0 || i > 13 = error $ "Invalid pin number: " ++ show i
      | True            = Pin i

data Mode = INPUT
          | OUTPUT
          | ANALOG
          | PWM
          | SERVO
          deriving Show

instance Firmata Mode where
   firmata INPUT  = 0
   firmata OUTPUT = 1
   firmata ANALOG = 2
   firmata PWM    = 3
   firmata SERVO  = 4
