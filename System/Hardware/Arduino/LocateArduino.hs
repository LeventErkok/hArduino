module System.Hardware.Arduino.LocateArduino(locateArduino) where

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Utils
import System.Hardware.Arduino.KnownBoards

import System.Hardware.Serialport

locateArduino :: Bool -> FilePath -> IO (Maybe Arduino)
locateArduino verbose fp = do
    dbg <- mkDebugPrinter verbose
    dbg $ "Trying to access arduino located at: " ++ show fp
    curPort <- openSerial fp defaultSerialSettings
    -- TODO: check that the handle has a good Arduino on the other side..
    return $ Just $ Arduino dbg (head knownBoards) curPort Nothing
