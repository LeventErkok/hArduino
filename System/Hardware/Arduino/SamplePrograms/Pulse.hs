-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Pulse
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Demonstrates 'pulseIn_hostTiming' and 'pulseOut_hostTiming' functions, sending
-- and receiving pulses to/from the board.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Pulse where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)

import System.Hardware.Arduino

-------------------------------------------------------------------------------
-- * Detecting pulses
-------------------------------------------------------------------------------

-- | Computes the amount of time a push-button is connected to
-- input pin 2 on the Arduino. We will wait for at most 5 seconds,
-- as a further demonstration of the time-out facility. Note that the
-- timing is done on the host side, so this measurement is inherently
-- inaccurate.
--
-- The wiring is straightforward: Simply put a push-button between
-- digital input 2 and +5V, guarded by a 10K resistor:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/PulseIn.png>>
pulseInDemo :: IO ()
pulseInDemo = withArduino False "/dev/cu.usbmodemFD131" $ do
               setPinMode pb INPUT
               go
 where pb = digital 2
       go = forever $ do
              liftIO $ putStr "Ready, push-and-hold for less than 5 seconds: "
              mbDur <- pulseIn_hostTiming pb True (Just 5000000)
              liftIO $ putStrLn $ case mbDur of
                Nothing -> "Time out!"
                Just d  -> "Button stayed high for: " ++ show d ++ " micro-seconds"

-------------------------------------------------------------------------------
-- * Sending pulses
-------------------------------------------------------------------------------

-- | Send pulses on a led as requested by the user. Note that the timing is computed
-- on the host side, thus the duration of the pulse is subject to some error due to
-- the Firmata communication overhead.
--
-- Wiring: Simply a led on pin 13:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Blink.png>>
pulseOutDemo :: IO ()
pulseOutDemo = withArduino False "/dev/cu.usbmodemFD131" $ do
              setPinMode led  OUTPUT
              digitalWrite led False
              forever trigger
 where led  = digital 13
       trigger = do liftIO $ putStr "Pulse duration? (microseconds) "
                    d <- liftIO getLine
                    case reads d of
                     [(v, "")] -> pulseOut_hostTiming led True 0 v
                     _         -> liftIO $ putStrLn "Please enter a number."
