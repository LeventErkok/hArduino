{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Comm where

import Control.Monad (when)
import qualified System.Hardware.Serialport as S
import qualified Data.ByteString as B

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Utils
import System.Hardware.Arduino.Protocol
import System.Hardware.Arduino.Firmata

withArduino :: Bool -> FilePath -> (Arduino -> IO ()) -> IO ()
withArduino verbose fp f =
        do debug <- mkDebugPrinter verbose
           debug $ "Accessing arduino located at: " ++ show fp
           S.withSerial fp S.defaultSerialSettings{S.commSpeed = S.CS57600} (mkArduino debug)
 where mkArduino debug port = do
          let recv = do debug "Waiting for a Sysex response.."
                        let skip = do b <- S.recv port 1
                                      case B.unpack b of
                                        []     -> skip
                                        [0xF0] -> return ()
                                        bs     -> do debug $ "Skipping bytes " ++ show (map showByte bs)
                                                     skip
                            collect sofar = do b <- S.recv port 1
                                               let rmsg = b : sofar
                                               if b == B.pack [0xF7] -- end message
                                                  then return $ reverse rmsg
                                                  else collect rmsg
                        skip
                        b <- B.concat `fmap` collect [B.pack [0xF0]]
                        let resp = unpackage b
                        debug $ "Received: " ++ show resp
                        return resp
              send msg = do debug $ "Sending: " ++ show msg
                            let p  = package msg
                                lp = B.length p
                            sent <- S.send port p
                            when (sent /= lp)
                                 (debug $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
          let a = Arduino debug port "ID: Uninitialized" (Just (ArduinoChannel recv send))
          (v1, v2, m) <- queryFirmware a
          f a{firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ m ++ ")"}
