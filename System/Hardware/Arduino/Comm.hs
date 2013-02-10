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
import Control.Concurrent   (MVar, myThreadId, ThreadId, throwTo, newChan, newMVar, newEmptyMVar, putMVar, writeChan, readChan, forkIO, modifyMVar_, tryTakeMVar, killThread)
import Control.Exception    (tryJust, AsyncException(UserInterrupt))
import Control.Monad.State  (runStateT, gets, liftIO, modify)
import Data.Bits            (testBit, (.&.))
import Data.List            (intercalate)
import Data.Maybe           (listToMaybe)
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))

import qualified Data.ByteString            as B (unpack, length)
import qualified Data.Map                   as M (empty, mapWithKey, insert, assocs, lookup)
import qualified Data.Set                   as S (empty)
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
           listenerTid <- newEmptyMVar
           let Arduino controller = do initialize listenerTid
                                       program
           S.withSerial fp S.defaultSerialSettings{S.commSpeed = S.CS57600} $ \port -> do
                let initBoardState = BoardState {
                                         boardCapabilities    = BoardCapabilities M.empty
                                       , analogReportingPins  = S.empty
                                       , digitalReportingPins = S.empty
                                       , pinStates            = M.empty
                                       , digitalWakeUpQueue   = []
                                       , lcds                 = M.empty
                                     }
                bs <- newMVar initBoardState
                dc <- newChan
                let initState = ArduinoState {
                                   message       = debugger
                                 , bailOut       = bailOut listenerTid
                                 , port          = port
                                 , firmataID     = "Unknown"
                                 , capabilities  = BoardCapabilities M.empty
                                 , boardState    = bs
                                 , deviceChannel = dc
                                 , listenerTid   = listenerTid
                              }
                res <- tryJust catchCtrlC $ runStateT controller initState
                case res of
                  Left () -> putStrLn "hArduino: Caught Ctrl-C, quitting.."
                  _       -> return ()
                cleanUp listenerTid
 where catchCtrlC UserInterrupt = Just ()
       catchCtrlC _             = Nothing
       cleanUp tid = do mbltid <- tryTakeMVar tid
                        case mbltid of
                          Just t -> killThread t
                          _      -> return ()
       bailOut tid m ms = do cleanUp tid
                             error $ "\n*** hArduino:ERROR: " ++ intercalate "\n*** " (m:ms)

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
setupListener :: Arduino ThreadId
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
                                    if b == firmataCmdVal END_SYSEX
                                       then return $ reverse sofar
                                       else collectSysEx (b : sofar)
            listener bs = do
                [cmd] <- getBytes 1
                resp  <- case getFirmataCmd cmd of
                           Left  unknown     -> return $ Unimplemented (Just (show unknown)) []
                           Right START_SYSEX -> unpackageSysEx `fmap` collectSysEx []
                           Right nonSysEx    -> unpackageNonSysEx getBytes nonSysEx
                case resp of
                  Unimplemented{}      -> dbg $ "Ignoring the received response: " ++ show resp
                  AnalogMessage mp l h -> modifyMVar_ bs $ \bst ->
                                           do -- the mp is indexed at 0; need to find the mapping
                                              let BoardCapabilities caps = boardCapabilities bst
                                                  mbP = listToMaybe [mappedPin | (mappedPin, (Just mp', _)) <- M.assocs caps, pinNo mp == mp']
                                              case mbP of
                                                Nothing -> return bst -- Mapping hasn't happened yet
                                                Just p  -> do
                                                   let v = (128 * fromIntegral (h .&. 0x07) + fromIntegral (l .&. 0x7f)) :: Int
                                                   case pinValue `fmap` (p `M.lookup` pinStates bst) of
                                                     Just (Just (Right v'))
                                                       | abs (v - v') < 10  -> return () -- be quiet, otherwise prints too much
                                                     _                      -> dbg $ "Updating analog pin " ++ show p ++ " values with " ++ showByteList [l,h] ++ " (" ++ show v ++ ")"
                                                   return bst{ pinStates = M.insert p PinData{pinMode = ANALOG, pinValue = Just (Right v)} (pinStates bst) }
                  DigitalMessage p l h -> do dbg $ "Updating digital port " ++ show p ++ " values with " ++ showByteList [l,h]
                                             modifyMVar_ bs $ \bst -> do
                                                  let upd o od | p /= pinPort o               = od   -- different port, no change
                                                               | pinMode od `notElem` [INPUT] = od   -- not an input pin, ignore
                                                               | True                         = od{pinValue = Just (Left newVal)}
                                                        where idx = pinPortIndex o
                                                              newVal | idx <= 6 = l `testBit` fromIntegral idx
                                                                     | True     = h `testBit` fromIntegral (idx - 7)
                                                  let wakeUpQ = digitalWakeUpQueue bst
                                                      bst' = bst{ pinStates          = M.mapWithKey upd (pinStates bst)
                                                                , digitalWakeUpQueue = []
                                                                }
                                                  mapM_ (`putMVar` ()) wakeUpQ
                                                  return bst'
                  _                    -> do dbg $ "Received " ++ show resp
                                             writeChan chan resp
        bs <- gets boardState
        tid <- liftIO $ forkIO $ forever (listener bs)
        debug $ "Started listener thread: " ++ show tid
        return tid

-- | Initialize our board, get capabilities, etc
initialize :: MVar ThreadId -> Arduino ()
initialize ltid = do
     -- Step 0: Set up the listener thread
     tid <- setupListener
     liftIO $ putMVar ltid tid
     -- Step 1: Send a reset to get things going
     send SystemReset
     -- Step 2: Send query-firmware, and wait until we get a response
     handshake QueryFirmware
               (\r -> case r of {Firmware{} -> True; _ -> False})
               (\(Firmware v1 v2 m) -> modify (\s -> s{firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ m ++ ")"}))
     -- Step 3: Send a capabilities request
     handshake CapabilityQuery
               (\r -> case r of {Capabilities{} -> True; _ -> False})
               (\(Capabilities c) -> modify (\s -> s{capabilities = c}))
     -- Step 4: Send analog-mapping query
     handshake AnalogMappingQuery
               (\r -> case r of {AnalogMapping{} -> True; _ -> False})
               (\(AnalogMapping as) -> do BoardCapabilities m <- gets capabilities
                                          -- need to put capabilities to both outer and inner state
                                          let caps = BoardCapabilities (M.mapWithKey (mapAnalog as) m)
                                          modify (\s -> s{capabilities = caps})
                                          bs <- gets boardState
                                          liftIO $ modifyMVar_ bs $ \bst -> return bst{boardCapabilities = caps})
     -- We're done, print capabilities in debug mode
     caps <- gets capabilities
     dbg <- gets message
     liftIO $ dbg $ "Handshake complete. Board capabilities:\n" ++ show caps
 where handshake msg isOK process = do
           dbg <- gets message
           send msg
           let wait = do resp <- recv
                         if isOK resp
                            then process resp
                            else do liftIO $ dbg $ "Skipping unexpected response: " ++ show resp
                                    wait
           wait
       mapAnalog bs p c
          | i < rl && m /= 0x7f
          = (Just m, snd c)
          | True             -- out-of-bounds, or not analog; ignore
          = c
         where rl = length bs
               i  = fromIntegral (pinNo p)
               m  = bs !! i
