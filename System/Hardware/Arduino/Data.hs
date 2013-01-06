{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Data where

import Data.Word
import Data.Maybe
import System.Hardware.Serialport

data ArduinoChannel = ArduinoChannel {
                  recv :: Int     -> IO String
                , send :: [Word8] -> IO ()
                }

data Board = Board {
               boardName       :: String
             , deviceVendorId  :: Word16
             , deviceProductId :: Word16
             }

data Arduino = Arduino {
                debug         :: String -> IO ()
              , board         :: Board
              , port          :: SerialPort
              , deviceChannel :: Maybe ArduinoChannel
              }

instance Show Arduino where
  show = boardName . board

getChannel :: Arduino -> ArduinoChannel
getChannel arduino@Arduino{deviceChannel} = fromMaybe die deviceChannel
  where die = error $ "Cannot communicate with board " ++ show arduino

class Firmata a where
  firmata :: a -> Word8

data Cmd = SetPinMode
         | DigitalMessage

instance Firmata Cmd where
   firmata SetPinMode     = 0xF4
   firmata DigitalMessage = 0x90

instance Firmata Bool where
   firmata False = 0
   firmata True  = 1
