-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Counter
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Demonstrates using two push-buttons to count up and down.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Counter where

import Control.Monad.Trans (liftIO)

import System.Hardware.Arduino

-- | Two push-button switches, controlling a counter value. We will increment
-- the counter if the first one ('bUp') is pressed, and decrement the value if the
-- second one ('bDown') is pressed. We also have a led connected to pin 13 (either use
-- the internal or connect an external one), that we light up when the counter value
-- is 0.
--
-- Wiring is very simple: Up-button connected to pin 4, Down-button connected
-- to pin 2, and a led on pin 13.
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Counter.png>>
counter :: IO ()
counter = withArduino False "/dev/cu.usbmodemfd131" $ do
            setPinMode led   OUTPUT
            setPinMode bUp   INPUT
            setPinMode bDown INPUT
            update (0::Int)
 where bUp   = pin 4
       bDown = pin 2
       led   = pin 13
       update curVal = do
                liftIO $ print curVal
                digitalWrite led (curVal == 0)
                [up, down] <- waitAnyHigh [bUp, bDown]
                let newVal = case (up, down) of
                               (True,  True)  -> curVal    -- simultaneous press
                               (True,  False) -> curVal+1
                               (False, True)  -> curVal-1
                               (False, False) -> curVal    -- can't happen
                update newVal
