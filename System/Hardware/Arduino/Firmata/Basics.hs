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
import Data.Word
import Data.Bits

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Parts
import System.Hardware.Arduino.Protocol
import System.Hardware.Arduino.Utils

queryFirmware :: Arduino (Word8, Word8, String)
queryFirmware = do
        send QueryFirmware
        r <- recv
        case r of
          Firmware v1 v2 m -> return (v1, v2, m)
          _                -> error $ "Got unexpected response for query firmware call: " ++ show r


delay :: Int -> Arduino ()
delay n = liftIO $ sleep (n `div` 1000)

setPinMode :: Pin -> Mode -> Arduino ()
setPinMode p m = send $ SetPinMode p m

digitalRead :: Pin -> Arduino (Mode, Bool)
digitalRead p = do
        send $ DigitalRead p
        r <- recv
        case r of
          DigitalPinState p' m b -> if p == p'
                                    then return (m, b)
                                    else error $ "Got unexpected response for " ++ show p' ++ " instead of " ++ show p
          _                -> error $ "Got unexpected response for query DigitalRead: " ++ show r

digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p v = do
        let (port, idx) = pinPort p
            dr cp@(Pin i)
             | i > 2 && i < 14 = digitalRead cp
             | True            = return (OUTPUT, False)
        oldVal <- map snd `fmap` mapM dr [Pin (fromIntegral (port * 8 + i)) | i <- [0 .. 7]]
        let [b0, b1, b2, b3, b4, b5, b6, b7] = [if i == idx then v else old | (old, i) <- zip oldVal [0 ..]]
            lsb = sum [1 `shiftL` i | (True, i) <- zip [b0, b1, b2, b3, b4, b5, b6, False] [0 .. 7]]
            msb = if b7 then 1 else 0
        send $ DigitalPortWrite port lsb msb
