-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Firmata.Basics
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Basic components of the firmata protocol
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata.Basics where

import Control.Monad.Trans (liftIO)
import Data.Word           (Word8)
import Data.Bits           (shiftL)

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Parts
import System.Hardware.Arduino.Protocol
import System.Hardware.Arduino.Comm
import qualified System.Hardware.Arduino.Utils as U

-- | Retrieve the Firmata firmware version running on the Arduino. The first
-- component is the major, second is the minor. The final value is a human
-- readable identifier for the particular board.
queryFirmware :: Arduino (Word8, Word8, String)
queryFirmware = do
        send QueryFirmware
        r <- recv
        case r of
          Firmware v1 v2 m -> return (v1, v2, m)
          _                -> error $ "queryFirmware: Got unexpected response for query firmware call: " ++ show r

-- | Delay the computaton for a given number of milli-seconds.
delay :: Int -> Arduino ()
delay = liftIO . U.delay

-- | Set the mode on a particular pin on the board.
setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p m = send $ SetPinMode p m

-- | Read the value of a pin in digital mode.
digitalRead :: Pin -> Arduino Bool
digitalRead p = do
        let (port, _) = pinPort p
        send $ DigitalReport port True
        send $ DigitalRead p
        r <- recv
        case r of
          DigitalPinState p' _ b -> if p == p'
                                    then return b
                                    else error $ "digitalRead: Got unexpected response for " ++ show p' ++ " instead of " ++ show p'
          _                     -> error $ "digitalRead: Got unexpected response for query DigitalReport: " ++ show r

-- | Set or clear a particular digital pin on the board.
digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p v = do
        let (port, idx) = pinPort p
            dr n
             | n > 2 && n < 14 = digitalRead (pin n)
             | True            = return False
        oldVal <- mapM dr [port * 8 + i | i <- [0 .. 7]]
        let [b0, b1, b2, b3, b4, b5, b6, b7] = [if i == idx then v else old | (old, i) <- zip oldVal [0 ..]]
            lsb = sum [1 `shiftL` i | (True, i) <- zip [b0, b1, b2, b3, b4, b5, b6, False] [0 .. 7]]
            msb = if b7 then 1 else 0
        send $ DigitalPortWrite port lsb msb
