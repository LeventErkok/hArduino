module Hardware.USBArduino.Data where

import Data.Word

data Board = Board {
               boardName       :: String
             , replPrompt      :: String
             , deviceVendorId  :: Word16
             , deviceProductId :: Word16
             }

instance Show Board where
  show = boardName
