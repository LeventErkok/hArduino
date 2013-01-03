module Hardware.USBArduino.Data where

import Data.Word

data Board = Board {
               boardName       :: String
             , deviceVendorId  :: Word16
             , deviceProductId :: Word16
             }
             deriving Show
