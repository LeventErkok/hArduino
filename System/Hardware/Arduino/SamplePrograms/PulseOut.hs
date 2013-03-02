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

-- | Send pulses on a led as requested by the user. Note that the timing is computed
-- on the host side, thus the duration of the pulse is subject to some error due to
-- the Firmata communication overhead.
--
-- Wiring: Simply a led on pin 13:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Blink.png>>
pulseDemo :: IO ()
pulseDemo = withArduino False "/dev/cu.usbmodemfd131" $ do
              setPinMode led  OUTPUT
              digitalWrite led False
              forever trigger
 where led  = digital 13
       trigger = do liftIO $ putStr "Pulse duration? (microseconds) "
                    d <- liftIO getLine
                    case reads d of
                     [(v, "")] -> pulseOut_hostTiming led True 0 v
                     _         -> liftIO $ putStrLn "Please enter a number."
