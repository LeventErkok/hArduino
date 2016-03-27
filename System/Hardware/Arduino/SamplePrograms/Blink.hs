-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Blink
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Blink where

import Control.Monad       (forever)

import System.Hardware.Arduino

-- | Blink the led connected to port 13 on the Arduino UNO board.
--
-- Note that you do not need any other components to run this example: Just hook
-- up your Arduino to the computer and make sure StandardFirmata is running on it.
-- However, you can connect a LED between Pin13 and GND if you want to blink an
-- external led as well, as depicted below:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Blink.png>>
blink :: IO ()
blink = withArduino False "/dev/cu.usbmodemFD131" $ do
           let led = digital 13
           setPinMode led OUTPUT
           forever $ do digitalWrite led True
                        delay 1000
                        digitalWrite led False
                        delay 1000
