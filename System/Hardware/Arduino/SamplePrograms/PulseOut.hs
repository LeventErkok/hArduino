-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.PulseOut
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Demonstrates the use of pulseOut
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.PulseOut where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)

import System.Hardware.Arduino

-- | Send pulses on a led as requested by the user.
--
-- Wiring: Simply a led on pin 13:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Blink.png>>
pulseDemo :: IO ()
pulseDemo = withArduino False "/dev/cu.usbmodemfd131" $ do
              setPinMode led  OUTPUT
              digitalWrite led False
              forever pulse
 where led  = digital 13
       pulse = do liftIO $ putStr "Pulse duration? (microseconds) "
                  d <- liftIO getLine
                  case reads d of
                   [(v, "")] -> pulseOut led True 0 v
                   _         -> liftIO $ putStrLn "Please enter a number."
