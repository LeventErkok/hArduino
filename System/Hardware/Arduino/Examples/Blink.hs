module System.Hardware.Arduino.Examples.Blink where

import Control.Concurrent (threadDelay)
import Control.Monad      (forever)
import System.Hardware.Arduino

-- Call it like this:
--
--      blink "/dev/cu.usbmodemfd131"
--
-- Where the file path is the device file the Arduino is connected to
blink :: FilePath -> IO ()
blink fp = withArduino True fp go
  where led = pin 13
        go arduino = forever $ do
           setPinMode arduino led OUTPUT
           b <- digitalRead arduino led
           putStrLn $ "Led is currently: " ++ show b
           digitalWrite arduino led True
           threadDelay 1000
           digitalWrite arduino led False
           threadDelay 1000
