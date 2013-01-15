-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Comm
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Basic serial communication routines
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Comm where

import Control.Monad.State
import qualified System.Hardware.Serialport as S
import qualified Data.ByteString as B

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Utils
import System.Hardware.Arduino.Protocol
import System.Hardware.Arduino.Firmata

withArduino :: Bool -> FilePath -> Arduino () -> IO ()
withArduino verbose fp program =
        do debugger <- mkDebugPrinter verbose
           debugger $ "Accessing arduino located at: " ++ show fp
           let Arduino controller = do (v1, v2, m) <- queryFirmware
                                       modify (\b -> b{firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ m ++ ")"})
                                       program
           S.withSerial fp S.defaultSerialSettings{S.commSpeed = S.CS57600} $ \port -> do
                _ <- runStateT controller (mkBoard debugger port)
                return ()
 where mkBoard debugger port = Board debugger port "ID: Uninitialized" (Just (ArduinoChannel recvChan recvNChan sendChan))
        where extract b = do let resp = unpackage b
                             debugger $ "Received: " ++ show resp
                             return resp
              recvChan = do debugger "Waiting for a Sysex response.."
                            let skip = do b <- S.recv port 1
                                          case B.unpack b of
                                            []     -> skip
                                            [0xF0] -> return ()
                                            bs     -> do debugger $ "Skipping bytes <" ++ unwords (map showByte bs) ++ ">"
                                                         skip
                                collect sofar = do b <- S.recv port 1
                                                   let rmsg = b : sofar
                                                   if b == B.pack [0xF7] -- end message
                                                      then return $ reverse rmsg
                                                      else collect rmsg
                            skip
                            chunks <- collect [B.pack [0xF0]]
                            extract $ B.concat chunks
              recvNChan n = do debugger $ "Waiting for a non-Sysex response of " ++ show n ++ " bytes"
                               let go need sofar
                                    | need <= 0  = return sofar
                                    | True       = do b <- S.recv port need
                                                      case B.length b of
                                                        0 -> go need sofar
                                                        l -> do when (need < l) $ debugger $ "Received partial response: <" ++ unwords (map showByte (B.unpack b)) ++ ">"
                                                                go (need - l) (b : sofar)
                               chunks <- go n []
                               extract $ B.concat $ reverse chunks
              sendChan msg = do let p  = package msg
                                    lp = B.length p
                                debugger $ "Sending: " ++ show msg ++ " <" ++ unwords (map showByte (B.unpack p)) ++ ">"
                                sent <- S.send port p
                                when (sent /= lp)
                                     (debugger $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
