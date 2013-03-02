-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.PulseIn
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Computes the time a button is held pressed, demonstrating the use of
-- the 'pulseIn_hostOnly' function with a time-out.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.PulseIn where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)

import System.Hardware.Arduino

-- | Computes the amount of time a push-button is connected to
-- input pin 2 on the Arduino. We will wait for at most 5 seconds,
-- as a further demonstration of the time-out facility. Note that the
-- timing is done on the host side, so this measurement is inherently
-- inaccurate.
--
-- The wiring is straightforward: Simply put a push-button between
-- digital input 2 and +5V, guarded by a 10K resistor:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Pulse.png>>
pulseDemo :: IO ()
pulseDemo = withArduino False "/dev/cu.usbmodemfd131" $ do
               setPinMode pb INPUT
               go
 where pb = digital 2
       go = forever $ do
              liftIO $ putStr "Ready, push-and-hold for less than 5 seconds: "
              mbDur <- pulseIn_hostTiming pb True (Just 5000000)
              liftIO $ putStrLn $ case mbDur of
                Nothing -> "Time out!"
                Just d  -> "Button stayed high for: " ++ show d ++ " micro-seconds"
