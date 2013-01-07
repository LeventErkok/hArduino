module System.Hardware.Arduino.Parts where

data Pin = Pin {pinVal :: Int}
         deriving Show

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
