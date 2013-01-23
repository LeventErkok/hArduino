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

import Control.Concurrent   (myThreadId, throwTo)
import Control.Exception    (tryJust, AsyncException(UserInterrupt))
import Control.Monad.State  (modify, runStateT, when)
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))

import qualified Data.ByteString            as B (pack, unpack, concat, length)
import qualified System.Hardware.Serialport as S (withSerial, defaultSerialSettings, CommSpeed(CS57600), commSpeed, recv, send)

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Utils
import System.Hardware.Arduino.Protocol
import System.Hardware.Arduino.Firmata

-- | Run the Haskell program to control the board:
--
--    * The file path argument should point to the device file that is
--      associated with the board. ('COM1' on Windows,
--      '/dev/cu.usbmodemfd131' on Mac, etc.)
--
--    * The boolean argument controls verbosity. It should remain
--      'False' unless you have communication issues. The print-out
--      is typically less-than-useful, but it might point to the root
--      cause of the problem.
--
-- See "System.Hardware.Arduino.Examples.Blink" for a simple example.
withArduino :: Bool       -- ^ If 'True', debugging info will be printed
            -> FilePath   -- ^ Path to the USB port
            -> Arduino () -- ^ The Haskell controller program to run
            -> IO ()
withArduino verbose fp program =
        do tid <- myThreadId
           _ <- installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing
           debugger <- mkDebugPrinter verbose
           debugger $ "Accessing arduino located at: " ++ show fp
           let Arduino controller = do (v1, v2, m) <- queryFirmware
                                       modify (\b -> b{firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ m ++ ")"})
                                       program
           S.withSerial fp S.defaultSerialSettings{S.commSpeed = S.CS57600} $ \port -> do
                res <- tryJust catchCtrlC $ runStateT controller (mkState debugger port)
                case res of
                  Left () -> putStrLn "hArduino: Caught Ctrl-C, quitting.."
                  _       -> return ()
 where catchCtrlC UserInterrupt = Just ()
       catchCtrlC _             = Nothing
       mkState debugger port = ArduinoState debugger port "ID: Uninitialized" (Just (ArduinoChannel recvChan recvNChan sendChan))
        where extract b = do let resp = unpackage b
                             debugger $ "Received: <" ++ unwords (map showByte (B.unpack b)) ++ ">: " ++ show resp
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
