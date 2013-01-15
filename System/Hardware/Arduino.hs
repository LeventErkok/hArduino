-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
-------------------------------------------------------------------------------
module System.Hardware.Arduino(
    Arduino
  , withArduino
  , pin, Mode(..)
  , module System.Hardware.Arduino.Firmata
 )
 where

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Comm
import System.Hardware.Arduino.Firmata
import System.Hardware.Arduino.Parts
