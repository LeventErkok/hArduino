module System.Hardware.Arduino.KnownBoards(knownBoards) where

import System.Hardware.Arduino.Data

arduinoUnoR3 :: Board
arduinoUnoR3 = Board {
          boardName       = "Arduino Uno R3"
        , deviceVendorId  = 0x2341
        , deviceProductId = 0x0043
        }

knownBoards :: [Board]
knownBoards = [arduinoUnoR3]
