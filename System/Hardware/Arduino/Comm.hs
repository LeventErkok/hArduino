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
          let extract b = do let resp = unpackage b
                             debug $ "Received: " ++ show resp
                             return resp
              recv = do debug "Waiting for a Sysex response.."
                        let skip = do b <- S.recv port 1
                                      case B.unpack b of
                                        []     -> skip
                                        [0xF0] -> return ()
                                        bs     -> do debug $ "Skipping bytes <" ++ unwords (map showByte bs) ++ ">"
                                                     skip
                            collect sofar = do b <- S.recv port 1
                                               let rmsg = b : sofar
                                               if b == B.pack [0xF7] -- end message
                                                  then return $ reverse rmsg
                                                  else collect rmsg
                        skip
                        chunks <- collect [B.pack [0xF0]]
                        extract $ B.concat chunks
              recvN n = do debug $ "Waiting for a non-Sysex response of " ++ show n ++ " bytes"
                           let go need sofar
                                | need <= 0  = return sofar
                                | True       = do b <- S.recv port need
                                                  case B.length b of
                                                    0 -> go need sofar
                                                    l -> do when (need < l) $ debug $ "Received partial response: <" ++ unwords (map showByte (B.unpack b)) ++ ">"
                                                            go (need - l) (b : sofar)
                           chunks <- go n []
                           extract $ B.concat $ reverse chunks
              send msg = do let p  = package msg
                                lp = B.length p
                            debug $ "Sending: " ++ show msg ++ " <" ++ unwords (map showByte (B.unpack p)) ++ ">"
                            sent <- S.send port p
                            when (sent /= lp)
                                 (debug $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
          let a = Arduino debug port "ID: Uninitialized" (Just (ArduinoChannel recv recvN send))
          (v1, v2, m) <- queryFirmware a
          f a{firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ m ++ ")"}
