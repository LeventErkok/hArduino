-- The mandatory "blink" example on Arduino

module System.Hardware.Arduino.Examples.Blink where

import Control.Monad           (forever)
import Control.Monad.Trans     (liftIO)
import System.Hardware.Arduino

blink :: IO ()
blink = withArduino False "/dev/cu.usbmodemfd131" $ do
           let led = pin 13
           setPinMode led OUTPUT
           forever $ do liftIO $ putStr "."
                        digitalWrite led True
                        delay 1000
                        digitalWrite led False
                        delay 1000
