module Hardware.HArduino.Data where

import Data.Word
import qualified System.USB as USB

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
              }

instance Show Arduino where
  show = boardName . board

data ArduinoChannel = ArduinoChannel {
                  recv :: USB.Size -> USB.Timeout -> IO (Maybe String)
                , send :: String   -> USB.Timeout -> IO Bool
                }
