-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Switch
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Reads the value of a push-button switch and displays it continuously
-- Demonstrates both normal switches, and switches using internal
-- pull-up resistors
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Switch where

import Control.Monad.Trans (liftIO)
import System.IO           (hSetBuffering, BufferMode(NoBuffering), stdout)

import System.Hardware.Arduino

-- | Read the value of a push-button switch (NO - normally open)
-- connected to input pin 2 on the Arduino. We will continuously
-- monitor and print the value as it changes. Also, we'll turn
-- the led on pin 7 on when the switch is pressed.
--
-- The wiring diagram is fairly straightforward:
--
--     Switch: ~10K pull-down resistor, between pin 2 and GND
--             Push-button NO-switch (normally open) between pin-2 and 5V
--
--     Led   : ~10K pull-down resistor between pin-7 and led+
--             Led between GND and the resistor
--
-- Don't neglect the resistors to make sure you don't do a short-circuit!
switch :: IO ()
switch = withArduino False "/dev/cu.usbmodemfd131" $ do
            liftIO $ hSetBuffering stdout NoBuffering
            setPinMode led OUTPUT
            setPinMode button INPUT
            go =<< digitalRead button
 where button = pin 2
       led    = pin 7
       go s   = do liftIO $ putStrLn $ "Button is currently " ++ if s then "ON" else "OFF"
                   digitalWrite led s
                   go =<< waitFor button

-- | Switch using internal pull-up resostors. Again, we read the value
-- of a push-button switch (NO - normally open), this time connected to
-- input pin 4 on the Arduino. We will continuously monitor and print the
-- value as it changes. Also, we'll turn the led on pin 7 on when the switch
-- is pressed. However, unlike the previous example where we used an
-- explicit resistor, we will instead utilize the internall pull-up resistor
-- attached to pin 4 of Arduino.
--
-- The wiring diagram is even simpler:
--
--     Switch: Push-bitton NO-switch (normally open) between pin 4 and GND
--
--     Led   : ~10K pull-down resistor between pin-7 and led+
--             Led between GND and the resistor
--
-- Note that the status of the pin will be reversed due to the pull-up resistor:
-- We will read a low when it is pressed, and high otherwise.
pullUpSwitch :: IO ()
pullUpSwitch = withArduino False "/dev/cu.usbmodemfd131" $ do
                  liftIO $ hSetBuffering stdout NoBuffering
                  setPinMode led OUTPUT
                  -- set-up this switch with a pull-up
                  setPinMode button INPUT
                  pullUpResistor button True
                  go =<< digitalRead button
 where button = pin 4
       led    = pin 7
       go s   = do liftIO $ putStrLn $ "Button is currently " ++ if s then "OFF" else "ON"
                   digitalWrite led (not s)  -- note the inversion
                   go =<< waitFor button

{-# ANN module "HLint: ignore Reduce duplication" #-}
