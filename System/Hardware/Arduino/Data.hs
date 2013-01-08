{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Data where

import Data.Maybe
import System.Hardware.Serialport
import System.Hardware.Arduino.Protocol

data ArduinoChannel = ArduinoChannel {
                  recv  :: IO Response
                , recvN :: Int -> IO Response
                , send  :: Request -> IO ()
                }

data Arduino = Arduino {
                debug         :: String -> IO ()
              , port          :: SerialPort
              , firmataID     :: String
              , deviceChannel :: Maybe ArduinoChannel
              }

instance Show Arduino where
  show = firmataID

getChannel :: Arduino -> ArduinoChannel
getChannel arduino@Arduino{deviceChannel} = fromMaybe die deviceChannel
  where die = error $ "Cannot communicate with board " ++ show arduino
