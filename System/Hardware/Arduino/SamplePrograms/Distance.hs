-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Distance
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Measuring distance using a HC-SR04 sensor. (Data sheet: <http://www.micropik.com/PDF/HCSR04.pdf>.)
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Distance where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import Numeric             (showGFloat)

import System.Hardware.Arduino

-- | Sound travels 340.29 meters per seconds. The echo time is round-trip, from the sensor
-- to the object and back. Thus, if echo is high for @d@ microseconds, then the distance in centimeters is:
--
--    @
--        d * 10^-6 * 340.29 * 10^2 / 2 
--      = 1.7e-2 * d
--    @
microSecondsToCentimeters :: Int -> Float
microSecondsToCentimeters d = 1.7e-2 * fromIntegral d

-- | Measure and display the distance continuously, as reported by an HC-SR04 sensor.
--
-- Wiring: Simply connect VCC and GND of HC-SR04 to Arduino as usual. The @Trig@ line on the sensor is connected
-- to Arduino pin 2. The @Echo@ line is connected to Arduino pin 3. We also have a led on pin 13 that we will light-up
-- if the distance detected is less than 2 centimeters, indicating an impending crash!
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Distance.png>>
distance :: IO ()
distance = withArduino True "/dev/cu.usbmodemfd131" $ do
             setPinMode led  OUTPUT
             setPinMode trig OUTPUT
             setPinMode echo INPUT
             update
 where trig = digital 2
       echo = digital 3
       led  = digital 13
       measure = do pulseOut trig True 10
                    Just d <- pulseIn echo True Nothing
                    let c = microSecondsToCentimeters d
                    liftIO $ putStrLn $ "Distance: " ++ showGFloat (Just 2) c " centimeters."
                    digitalWrite led (c < 2)
       update = forever $ do measure
                             delay 1000
