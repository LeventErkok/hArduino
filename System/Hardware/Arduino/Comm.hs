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

import Control.Monad        (when, forever)
import Control.Concurrent   (myThreadId, throwTo, newChan, newEmptyMVar, modifyMVar_, writeChan, readChan, forkIO)
import Control.Exception    (tryJust, AsyncException(UserInterrupt))
import Control.Monad.State  (runStateT, gets, liftIO, modify)
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))

import qualified Data.ByteString            as B (unpack, length)
import qualified System.Hardware.Serialport as S (withSerial, defaultSerialSettings, CommSpeed(CS57600), commSpeed, recv, send)

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Utils
import System.Hardware.Arduino.Protocol

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
           let Arduino controller = do initialize
                                       program
           S.withSerial fp S.defaultSerialSettings{S.commSpeed = S.CS57600} $ \port -> do
                bs <- newEmptyMVar
                dc <- newChan
                res <- tryJust catchCtrlC $ runStateT controller (ArduinoState debugger port "Uninitialized" bs dc)
                case res of
                  Left () -> putStrLn "hArduino: Caught Ctrl-C, quitting.."
                  _       -> return ()
 where catchCtrlC UserInterrupt = Just ()
       catchCtrlC _             = Nothing

-- | Send down a request.
send :: Request -> Arduino ()
send req = do debug $ "Sending: " ++ show req ++ " <" ++ unwords (map showByte (B.unpack p)) ++ ">"
              serial <- gets port
              sent <- liftIO $ S.send serial p
              when (sent /= lp)
                   (debug $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
   where p  = package req
         lp = B.length p

-- | Receive a sys-ex response. This is a blocking call.
recv :: Arduino Response
recv = do ch <- gets deviceChannel
          liftIO $ readChan ch

-- | Start a thread to listen to the board and populate the channel with incoming queries.
-- NB. This function is run in a thread; so be careful not to throw error or die otherwise
-- in here.
setupListener :: Arduino ()
setupListener = do
        serial <- gets port
        dbg    <- gets message
        chan   <- gets deviceChannel
        let getBytes n = do let go need sofar
                                 | need <= 0  = return $ reverse sofar
                                 | True       = do b <- S.recv serial need
                                                   case B.length b of
                                                     0 -> go need sofar
                                                     l -> go (need - l) (b : sofar)
                            chunks <- go n []
                            return $ concatMap B.unpack chunks
            collectSysEx sofar = do [b] <- getBytes 1
                                    if b == 0xF7 -- end sysex
                                       then return $ reverse sofar
                                       else collectSysEx (b : sofar)
            listener = do [cmd] <- getBytes 1
                          resp  <- case getFirmataCmd cmd of
                                     START_SYSEX      -> do bs <- collectSysEx []
                                                            case bs of
                                                              []          -> return $ Unknown bs
                                                              (sc:scargs) -> case (getSysExCommand sc, scargs) of
                                                                               (REPORT_FIRMWARE, majV : minV : rest) -> return $ Firmware majV minV (getString rest)
                                                                               _                 -> error $ "TBD/Sysex: " ++ showByte sc
                                     ANALOG_MESSAGE _ -> do bs@[_lsb, _msb] <- getBytes 2
                                                            let resp = Unknown bs
                                                            dbg $ "ANALOG_MESSAGE Received: <" ++ unwords (map showByte (cmd:bs)) ++ ">: " ++ show resp
                                                            return resp
                                     PROTOCOL_VERSION -> do bs@[_lsb, _msb] <- getBytes 2
                                                            let resp = Unknown bs
                                                            dbg $ "PROTOCOL_VERSION Received: <" ++ unwords (map showByte (cmd:bs)) ++ ">: " ++ show resp
                                                            return resp
                                     _                 -> error $ "TBD/Firmata: " ++ showByte cmd
                          case resp of
                            Unknown _ -> return ()
                            _         -> writeChan chan resp
        tid <- liftIO $ forkIO $ forever listener
        debug $ "Started listener thread: " ++ show tid

-- | Initialize our board, get capabilities, set up comms
initialize :: Arduino ()
initialize = do
     dbg <- gets message
     -- Step 1: Set up the listener thread
     setupListener
     -- Step 2: Send query-firmware, and wait until we get a response
     send QueryFirmware
     let waitFW = do liftIO $ dbg "Waiting for Firmware response."
                     r <- recv
                     case r of
                       Firmware v1 v2 m -> do liftIO $ dbg $ "Got Firmware response " ++ show r
                                              modify (\s -> s{firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ m ++ ")"})
                       _                -> do liftIO $ dbg $ "Skipping unexpected response: " ++ show r
                                              waitFW
     waitFW
     -- Step 3: Send a capabilities request
     send CapabilityQuery
     let waitCQ = do liftIO $ dbg "Waiting for capabilities request."
                     r <- recv
                     case r of
                       Capabilities c -> do liftIO $ dbg "Got capabilities response!"
                                            s <- gets boardState
                                            liftIO $ modifyMVar_ s (\b -> return (b{capabilities = c}))
                       _              -> do liftIO $ dbg $ "Skipping unexpected response: " ++ show r
                                            waitCQ
     waitCQ
