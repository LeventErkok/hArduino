{-# LANGUAGE NamedFieldPuns #-}
module Hardware.HArduino.Data where

import Data.Word
import Data.Maybe
import qualified System.USB as USB

data ArduinoChannel = ArduinoChannel {
                  recv :: USB.Size -> USB.Timeout -> IO (Maybe String)
                , send :: String   -> USB.Timeout -> IO Bool
                }

data Board = Board {
               boardName       :: String
             , deviceVendorId  :: Word16
             , deviceProductId :: Word16
             }

data Arduino = Arduino {
                board      :: Board
              , device     :: USB.Device
              , deviceDesc :: USB.DeviceDesc
              , context    :: USB.Ctx
              , deviceChannel   :: Maybe ArduinoChannel
              }

instance Show Arduino where
  show = boardName . board

getChannel :: Arduino -> ArduinoChannel
getChannel arduino@Arduino{deviceChannel} = fromMaybe die deviceChannel
  where die = error $ "Cannot communicate with board " ++ show arduino
