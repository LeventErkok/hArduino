-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Counter
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Demonstrates using two switches to count up/down, controlled from
-- different Haskell threads
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Counter where

import Control.Monad.Trans (liftIO)

import System.Hardware.Arduino

-- | Two push-button switches, wired up very similar to the set up
-- in "System.Hardware.Arduino.SamplePrograms.Switch", one on pin 2 ('bDown'),
-- second on pin 4 ('bUp'); both with explicit resistors.
--
-- We will keep a counter, incrementing it by 1 if 'bUp'
-- is pressed; and decrementing it if 'bDown' is pressed;
-- thus implementing a simple counter.
counter :: IO ()
counter = withArduino False "/dev/cu.usbmodemfd131" $ do
            setPinMode bUp INPUT
            setPinMode bDown INPUT
            update (0::Int)
 where bUp   = pin 4
       bDown = pin 2
       update curVal = do
                liftIO $ print curVal
                [up, down] <- waitAnyHigh [bUp, bDown]
                let newVal = case (up, down) of
                               (True,  True)  -> curVal    -- simultaneous press
                               (True,  False) -> curVal+1
                               (False, True)  -> curVal-1
                               (False, False) -> curVal    -- can't happen
                update newVal
