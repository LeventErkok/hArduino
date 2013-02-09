-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Button
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Reads the value of a push-button and displays it's status continuously
-- on the computer screen and by blinking a led on the Arduino.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Button where

import Control.Monad.Trans (liftIO)
import System.IO           (hSetBuffering, BufferMode(NoBuffering), stdout)

import System.Hardware.Arduino

-- | Read the value of a push-button (NO - normally open)
-- connected to input pin 2 on the Arduino. We will continuously
-- monitor and print the value as it changes. Also, we'll turn
-- the led on pin 13 on when the switch is pressed.
--
-- The wiring is straightforward: Simply put a push-button between
-- digital input 2 and +5V, guarded by a 10K resistor:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Button.png>>
button :: IO ()
button = withArduino False "/dev/cu.usbmodemfd131" $ do
            liftIO $ hSetBuffering stdout NoBuffering
            setPinMode led OUTPUT
            setPinMode but INPUT
            go =<< digitalRead but
 where but  = pin 2
       led  = pin 13
       go s = do liftIO $ putStrLn $ "Button is currently " ++ if s then "ON" else "OFF"
                 digitalWrite led s
                 go =<< waitFor but
