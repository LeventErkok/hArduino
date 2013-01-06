module Hardware.HArduino.KnownBoards(knownBoards) where

import Hardware.HArduino.Data

arduinoUnoR3 :: Board
arduinoUnoR3 = Board {
          boardName       = "Arduino Uno R3"
        , replPrompt      = "Uno"
        , deviceVendorId  = 0x2341
        , deviceProductId = 0x0043
        }

knownBoards :: [Board]
knownBoards = [arduinoUnoR3]
