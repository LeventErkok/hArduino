-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Distance
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Measuring distance using HC-SR04 sensor
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.Distance where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import Numeric             (showGFloat)

import System.Hardware.Arduino

-- | Sound travels 340.29 meters per seconds. The echo time is round-trip, from the sensor
-- to the object and back. Thus, if echo is high for d microsecond, the distance in centimeters is:
-- @d * 10^-6 * 340.29 * 10^2 / 2 = 1.7e-2 * d@.
microSecondsToCentimeters :: Int -> Float
microSecondsToCentimeters d = 1.7e-2 * fromIntegral d

distance :: IO ()
distance = withArduino True "/dev/cu.usbmodemfd131" $ do
             setPinMode trig OUTPUT
             setPinMode echo INPUT
             digitalWrite trig False
             update
 where trig = digital 13
       echo = digital 12
       trigger = do digitalWrite trig True
                    delay 1
                    digitalWrite trig False
       measure = do trigger
                    Just d <- pulseIn echo True Nothing
                    let c = microSecondsToCentimeters d
                    liftIO $ putStrLn $ "Distance: " ++ showGFloat (Just 2) c " centimeters."
       update = forever $ do measure
                             delay 1000
