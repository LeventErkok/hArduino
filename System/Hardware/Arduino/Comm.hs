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

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module System.Hardware.Arduino.Comm where

import Control.Monad        (when, forever)
import Control.Concurrent   (MVar, ThreadId, newChan, newMVar, newEmptyMVar, putMVar, writeChan, readChan, forkIO, modifyMVar_, tryTakeMVar, killThread)
import Control.Exception    (tryJust, AsyncException(UserInterrupt), handle, SomeException)
import Control.Monad.State  (runStateT, gets, liftIO, modify)
import Data.Bits            (testBit, (.&.))
import Data.List            (intercalate, isInfixOf)
import Data.Maybe           (listToMaybe)
import Data.Word            (Word8)
import System.Timeout       (timeout)
import System.IO            (stderr, hPutStrLn)

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
--      associated with the board. (@COM1@ on Windows,
--      @/dev/cu.usbmodemFD131@ on Mac, etc.)
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
        do debugger <- mkDebugPrinter verbose
           debugger $ "Accessing arduino located at: " ++ show fp
           lTid <- newEmptyMVar
           let Arduino controller = do initOK <- initialize lTid
                                       if initOK
                                          then program
                                          else error "Communication time-out (5s) expired."
           handle (\(e::SomeException) -> do cleanUp lTid
                                             let selfErr = "*** hArduino" `isInfixOf` show e
                                             hPutStrLn stderr $ if selfErr
                                                                then dropWhile (== '\n') (show e)
                                                                else "*** hArduino:ERROR: " ++ show e
                                                                     ++ concatMap ("\n*** " ++) [ "Make sure your Arduino is connected to " ++ fp
                                                                                                , "And StandardFirmata is running on it!"
                                                                                                ]) $
             S.withSerial fp S.defaultSerialSettings{S.commSpeed = S.CS57600} $ \curPort -> do
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
                                 , bailOut       = bailOutF lTid
                                 , port          = curPort
                                 , firmataID     = "Unknown"
                                 , capabilities  = BoardCapabilities M.empty
                                 , boardState    = bs
                                 , deviceChannel = dc
                                 , listenerTid   = lTid
                              }
                res <- tryJust catchCtrlC $ runStateT controller initState
                case res of
                  Left () -> putStrLn "hArduino: Caught Ctrl-C, quitting.."
                  _       -> return ()
                cleanUp lTid
 where catchCtrlC UserInterrupt = Just ()
       catchCtrlC _             = Nothing

       cleanUp tid = do mbltid <- tryTakeMVar tid
                        maybe (pure ()) killThread mbltid

       bailOutF tid m ms = do cleanUp tid
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

-- | Receive a sys-ex response with time-out. This is a blocking call, and will wait until
-- either the time-out expires or the message is received
recvTimeOut :: Int -> Arduino (Maybe Response)
recvTimeOut n = do ch <- gets deviceChannel
                   liftIO $ timeout n (readChan ch)

-- | Start a thread to listen to the board and populate the channel with incoming queries.
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
                  -- NB. When Firmata sends back AnalogMessage, it uses the number in A0-A1-A2, etc., i.e., 0-1-2; which we
                  -- need to properly interpret in our own pin mapping schema, where analogs come after digitals.
                  AnalogMessage mp l h -> modifyMVar_ bs $ \bst ->
                                           do let BoardCapabilities caps = boardCapabilities bst
                                                  mbP = listToMaybe [mappedPin | (mappedPin, PinCapabilities{analogPinNumber = Just mp'}) <- M.assocs caps, pinNo mp == mp']
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
                                                  let upd o od | p /= pinPort o      = od   -- different port, no change
                                                               | pinMode od /= INPUT = od   -- not an input pin, ignore
                                                               | True                = od{pinValue = Just (Left newVal)}
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

-- | Initialize our board, get capabilities, etc. Returns True if initialization
-- went OK, False if not.
initialize :: MVar ThreadId -> Arduino Bool
initialize ltid = do
     -- Step 0: Set up the listener thread
     tid <- setupListener
     liftIO $ putMVar ltid tid
     -- Step 1: Send a reset to get things going
     send SystemReset
     -- Step 2: Send query-firmware, and wait until we get a response
     -- To accommodate for the case when standard-Firmata may not be running,
     -- we will time out after 10 seconds of waiting, which should be plenty
     mbTo <- handshake QueryFirmware (Just (5000000 :: Int))
                       (\case Firmware{} -> True
                              _          -> False)
                       (\(Firmware v1 v2 m) -> modify (\s -> s{firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ m ++ ")"}))
     case mbTo of
       Nothing -> return False  -- timed out
       Just () -> do -- Step 3: Send a capabilities request
                     _ <- handshake CapabilityQuery Nothing
                                    (\case Capabilities{} -> True
                                           _              -> False)
                                    (\(Capabilities c) -> modify (\s -> s{capabilities = c}))
                     -- Step 4: Send analog-mapping query
                     _ <- handshake AnalogMappingQuery Nothing
                                    (\case AnalogMapping{} -> True
                                           _               -> False)
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
                     return True
 where handshake msg mbTOut isOK process = do
           dbg <- gets message
           send msg
           let wait = do mbResp <- case mbTOut of
                                     Nothing -> Just `fmap` recv
                                     Just n  -> recvTimeOut n
                         case mbResp of
                           Nothing   -> return Nothing
                           Just resp -> if isOK resp
                                        then Just `fmap` process resp
                                        else do liftIO $ dbg $ "Skipping unexpected response: " ++ show resp
                                                wait
           wait
       mapAnalog :: [Word8] -> IPin -> PinCapabilities -> PinCapabilities
       mapAnalog as p c
          | i < rl && m /= 0x7f
          = c{analogPinNumber = Just m}
          | True             -- out-of-bounds, or not analog; ignore
          = c
         where rl = length as
               i  = fromIntegral (pinNo p)
               m  = as !! i
