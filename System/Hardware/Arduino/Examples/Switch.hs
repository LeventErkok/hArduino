-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Examples.Switch
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Reads the value of a push-button switch and displays it continuously
-------------------------------------------------------------------------------

module System.Hardware.Arduino.Examples.Switch where

import Control.Monad           (forever)
import Control.Monad.Trans     (liftIO)
import System.IO               (hSetBuffering, BufferMode(NoBuffering), stdout)

import System.Hardware.Arduino

-- | Read the value of a push-button switch (NO - normally open)
-- connected to input pin 2 on the Arduino. We will continuously
-- monitor and print the value as it changes. Also, we'll turn
-- the led on pin 13 on when the switch is pressed.
--
-- The wiring diagram is fairly straightforward:
--
--     ~10K pull-down resistor, between pin 2 and GND
--     Push-button switch between pin-2 and 5V
--
-- Don't neglect the pull-down resistor to make sure you don't
-- do a short-circuit between GND and 5V!
switch :: IO ()
switch = withArduino True "/dev/cu.usbmodemfd131" $ do
            liftIO $ hSetBuffering stdout NoBuffering
            -- setPinMode led OUTPUT
            setPinMode button INPUT
            -- current <- digitalRead button
            let current = True
            report (not current) current
            go current
 where button = pin 2
       -- led    = pin 13
       -- We're using a NO (normally open) button with a pull-down resistor,
       -- so if new is True then the button is pressed
       report prev new
         | prev == new = return ()  -- No state change, nothing to report
         | new         = liftIO $ putStrLn "Button is currently ON"
         | True        = liftIO $ putStrLn "Button is currently OFF"
       go prev = forever $ do let new = False
                              -- new <- digitalRead button
                              -- digitalWrite led new
                              report prev new
                              delay 1000
                              go new
