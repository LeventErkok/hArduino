{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Comm where

import qualified Control.Exception as E
import Data.Char     (chr)
import qualified System.Hardware.Serialport as S
import qualified Data.ByteString as B

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.LocateArduino

withArduino :: Bool -> FilePath -> (Arduino -> IO a) -> IO a
withArduino verbose fp f =
        do mbA <- locateArduino verbose fp
           case mbA of
             Nothing -> error "Cannot connect to Arduino."
             Just a@Arduino{debug, port}
                -> E.bracket_ (return ())
                              (do debug "Closing the serial port."
                                  S.closeSerial port)
                              (f (addChannels a))
 where addChannels arduino@Arduino{debug, port} = arduino{deviceChannel = Just (ArduinoChannel recv send)}
          where recv cnt = do debug $ "Receiving " ++ show cnt ++ " bytes."
                              b <- S.recv port cnt
                              return $ map (chr . fromIntegral) $ B.unpack b
                send msg = do let str = map (chr . fromIntegral) msg
                              debug $ "Sending: " ++ show str
                              sent <- S.send port $ B.pack msg
                              case sent `compare` length msg of
                                EQ -> return ()
                                LT -> debug $  "Send failed, tried to send " ++ show (length msg)
                                            ++ " but only " ++ show sent ++ " were sent."
                                GT -> debug $  "Send failed, tried to send " ++ show (length msg)
                                            ++ " but system reports " ++ show sent ++ " were sent."
