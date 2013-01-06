module Hardware.HArduino.Parts where 

data Pin = Pin {pinVal :: Int}

instance Show Pin where
  show (Pin i) | i < 10 = '0' : show i
               | True   = show i

-- 13 pins is UNO specific, should really have the board here
pin :: Int -> Pin
pin i | i < 0 || i > 13 = error $ "Invalid pin number: " ++ show i
      | True            = Pin i

data Mode = INPUT
          | OUTPUT
          | ANALOG
          | PWM
          | SERVO

instance Show Mode where
  show INPUT  = "i"
  show OUTPUT = "o"
  show ANALOG = "a"
  show PWM    = "p"
  show SERVO  = "s"
