module System.Hardware.Arduino.Examples.Blink where

import Control.Monad      (forever, void)
import System.Hardware.Arduino

-- threadDelay is broken on Mac!
--   see: http://hackage.haskell.org/trac/ghc/ticket/7299
-- so use sleep, jeez..
import System.Process (system)

sleep :: Int -> IO ()
sleep n = void $ system $ "sleep " ++ show n

blink :: IO ()
blink = withArduino False "/dev/cu.usbmodemfd131" go
  where led = pin 13
        go arduino = forever $ do
           setPinMode arduino led OUTPUT
           putStrLn "ON"
           digitalWrite arduino led True
           sleep 1
           putStrLn "OFF"
           digitalWrite arduino led False
           sleep 1
