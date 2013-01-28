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
            current <- digitalRead button
            report (not current) current
            go current
 where button = pin 2
       led    = pin 7
       report prev new
         | prev /= new = do liftIO $ putStrLn $ "Button is currently " ++ if new then "ON" else "OFF"
                            digitalWrite led new
         | True        = return ()
       go prev = do new <- waitFor button
                    report prev new
                    go new
