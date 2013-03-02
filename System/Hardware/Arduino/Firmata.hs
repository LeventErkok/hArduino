-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Firmata
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Implementation of the firmata protocol
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Firmata where

import Control.Concurrent  (newEmptyMVar, readMVar, withMVar, modifyMVar_, threadDelay)
import Control.Monad       (when, unless, void)
import Control.Monad.State (StateT(..), gets)
import Control.Monad.Trans (liftIO)
import Data.Bits           ((.&.), shiftR, setBit)
import Data.Maybe          (fromMaybe)
import Data.Time           (getCurrentTime, utctDayTime)
import System.Timeout      (timeout)
import Data.Word           (Word8)

import qualified Data.Map as M

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Comm

import qualified System.Hardware.Arduino.Utils as U

-- | Retrieve the Firmata firmware version running on the Arduino. The first
-- component is the major, second is the minor. The final value is a human
-- readable identifier for the particular board.
queryFirmware :: Arduino (Word8, Word8, String)
queryFirmware = do
        send QueryFirmware
        r <- recv
        case r of
          Firmware v1 v2 m -> return (v1, v2, m)
          _                -> die "queryFirmware: Got unexpected response for query firmware call: " [show r]

-- | Delay the computaton for a given number of milli-seconds
delay :: Int -> Arduino ()
delay = liftIO . U.delay

-- | Time a given action, result is measured in micro-seconds.
time :: Arduino a -> Arduino (Int, a)
time a = do start <- tick
            r     <- a
            end   <- r `seq` tick
            return (toMicroSeconds (end - start), r)
 where -- tick gets the current time in picoseconds
       tick = do t <- liftIO $ utctDayTime `fmap` getCurrentTime
                 let precision = 1000000000000 :: Integer
                 return . round . (fromIntegral precision *) . toRational $ t
       toMicroSeconds :: Integer -> Int
       toMicroSeconds t = fromIntegral $ t `quot` 1000000

-- | Time-out a given action. Time-out amount is in micro-seconds.
timeOut :: Int -> Arduino a -> Arduino (Maybe a)
timeOut to (Arduino (StateT f)) = Arduino (StateT (\st -> do
        mbRes <- timeout to (f st)
        case mbRes of
          Nothing       -> return (Nothing, st)
          Just (a, st') -> return (Just a,  st')))

-- | Set the mode on a particular pin on the board
setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p' m = do
   p <- getInternalPin p'
   extras <- registerPinMode p m
   send $ SetPinMode p m
   mapM_ send extras

-- | Set or clear a digital pin on the board
digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p' v = do
   (p, pd) <- convertAndCheckPin "digitalWrite" p' OUTPUT
   case pinValue pd of
     Just (Left b) | b == v -> return () -- no change, nothing to do
     _                      -> do (lsb, msb) <- computePortData p v
                                  send $ DigitalPortWrite (pinPort p) lsb msb

-- | Turn on/off internal pull-up resistor on an input pin
pullUpResistor :: Pin -> Bool -> Arduino ()
pullUpResistor p' v = do
   (p, _) <- convertAndCheckPin "pullUpResistor" p' INPUT
   (lsb, msb) <- computePortData p v
   send $ DigitalPortWrite (pinPort p) lsb msb

-- | Read the value of a pin in digital mode; this is a non-blocking call, returning
-- the current value immediately. See 'waitFor' for a version that waits for a change
-- in the pin first.
digitalRead :: Pin -> Arduino Bool
digitalRead p' = do
   (_, pd) <- convertAndCheckPin "digitalRead" p' INPUT
   return $ case pinValue pd of
              Just (Left v) -> v
              _             -> False -- no (correctly-typed) value reported yet, default to False

-- | Wait for a change in the value of the digital input pin. Returns the new value.
-- Note that this is a blocking call. For a non-blocking version, see 'digitalRead', which returns the current
-- value of a pin immediately.
waitFor :: Pin -> Arduino Bool
waitFor p = head `fmap` waitAny [p]

-- | Wait for a change in any of the given pins. Once a change is detected, all the new values are
-- returned. Similar to 'waitFor', but is useful when we are watching multiple digital inputs.
waitAny :: [Pin] -> Arduino [Bool]
waitAny ps = map snd `fmap` waitGeneric ps

-- | Wait for any of the given pins to go from low to high. If all of the pins are high to start
-- with, then we first wait for one of them to go low, and then wait for one of them to go back high.
-- Returns the new values.
waitAnyHigh :: [Pin] -> Arduino [Bool]
waitAnyHigh ps = do
   curVals <- mapM digitalRead ps
   when (and curVals) $ void $ waitAnyLow ps   -- all are H to start with, wait for at least one to go low
   vs <- waitGeneric ps  -- wait for some change
   if (False, True) `elem` vs
      then return $ map snd vs
      else waitAnyHigh ps

-- | Wait for any of the given pins to go from high to low. If all of the pins are low to start
-- with, then we first wait for one of them to go high, and then wait for one of them to go back low.
-- Returns the new values.
waitAnyLow :: [Pin] -> Arduino [Bool]
waitAnyLow ps = do
   curVals <- mapM digitalRead ps
   unless (or curVals) $ void $ waitAnyHigh ps   -- all are L to start with, wait for at least one to go high
   vs <- waitGeneric ps  -- wait for some change
   if (True, False) `elem` vs
      then return $ map snd vs
      else waitAnyLow ps

-- | A utility function, waits for any change on any given pin
-- and returns both old and new values. It's guaranteed that
-- at least one returned pair have differing values.
waitGeneric :: [Pin] -> Arduino [(Bool, Bool)]
waitGeneric ps = do
   curVals <- mapM digitalRead ps
   semaphore <- liftIO newEmptyMVar
   let wait = do digitalWakeUp semaphore
                 liftIO $ readMVar semaphore
                 newVals <- mapM digitalRead ps
                 if curVals == newVals
                    then wait
                    else return $ zip curVals newVals
   wait

-- | Send down a pulse, and measure how long the pin reports a corresponding pulse, with a potential time-out. The call @pulse p v duration mbTimeOut@
-- does the following:
--
--   * Set the pin to value @v@ for @duration@ microseconds.
--
--   * Waits 2 microseconds
--
--   * Waits until pin @p@ has value @not v@.
--
--   * Returns, in micro-seconds, the duration the pin stayed @v@, counting from the 2 microsecond wait.
--
-- Time-out parameter is used as follows:
--
--    * If @mbTimeOut@ is @Nothing@, then 'pulse' will wait until the pin attains the value required and so long as it holds it.
--    Note that very-long time-out values are unlikely to be accurate.
-- 
--    * If @mbTimeOut@ is @Just t@ then, 'pulse' will stop if the above procedure does not complete within the given micro-seconds.
--    In this case, the overall return value is @Nothing@.
--
-- NB. Both the time-out value and the return value are given in micro-seconds.
--
-- NB. As of March 2 2013; StandardFirmata that's distributed with the Arduino-App does /not/ support the Pulse-In command.
-- However, there is a patch to add this command; see: <http://github.com/rwldrn/johnny-five/issues/18> for details.
-- If you want to use hArduino's 'pulseIn' command, then you /have/ to install the above patch. Also see the function
-- 'pulseIn_hostOnly', which works with the distributed StandardFirmata: It implements a version that is not as
-- accurate in its timing, but might be sufficient if high precision is not required.
pulse :: Pin -> Bool -> Int -> Maybe Int -> Arduino (Maybe Int)
pulse p' v duration mbTo = do
        (p, _) <- convertAndCheckPin "pulse" p' INPUT
        let to = fromMaybe maxAllowed mbTo
            maxAllowed = 2147483647  -- works out to about 36 minutes; which is way beyond the accuracy provided by Arduino
            bad x = x < 0 || x > maxAllowed
        when (any bad [duration, to]) $ die ("Invalid duration/time-out values for pulse on pin " ++ show p)
                                            [ "Values should be between 0 and " ++ show maxAllowed
                                            , "Received: " ++ show (duration, to)
                                            ]
        send $ Pulse p v (fromIntegral duration) (fromIntegral to)
        r <- recv
        case r of
          PulseResponse pOut d | p == pOut -> case d of
                                                0 -> return  Nothing
                                                i -> return (Just (fromIntegral i))
          _                                -> die ("pulseIn: Got unexpected response for Pulse call on pin: " ++ show p') [show r]

-- | A /hostOnly/ version of pulse-out on a digital-pin. Use this function only for cases where the
-- precision required only matters for the host, not for the board. That is, due to the inherent
-- delays involved in Firmata communication, the timing will /not/ be accurate, and should not
-- be expected to work uniformly over different boards. Similar comments apply for 'pulseIn_hostTiming'
-- as well. See the function 'pulse' for a more accurate version.
pulseOut_hostTiming :: Pin  -- ^ Pin to send the pulse on
                  -> Bool -- ^ Pulse value
                  -> Int  -- ^ Time, in microseconds, to signal beginning of pulse; will send the opposite value for this amount
                  -> Int  -- ^ Pulse duration, measured in microseconds
                  -> Arduino ()
pulseOut_hostTiming p' pulseValue dBefore dAfter
  | dBefore < 0 || dAfter < 0
  = die ("pulseOut: Invalid delay amounts: " ++ show (dBefore, dAfter)) 
        [ "Pre-delay and pulse-amounts must be non-negative."]
  | True
  = do (p, pd) <- convertAndCheckPin "pulseOut_hostTiming" p' OUTPUT
       let curPort  = pinPort p
           curIndex = pinPortIndex p
       bs  <- gets boardState
       (setMask, resetMask) <- liftIO $ withMVar bs $ \bst -> do
           let values = [(pinPortIndex sp, pinValue spd) | (sp, spd) <- M.assocs (pinStates bst), curPort == pinPort sp, pinMode pd `elem` [INPUT, OUTPUT]]
               getVal nv i
                | i == curIndex                              = nv
                | Just (Just (Left ov)) <- i `lookup` values = ov
                | True                                       = False
               mkMask val = let [b0, b1, b2, b3, b4, b5, b6, b7] = map (getVal val) [0 .. 7]
                                lsb = foldr (\(i, b) m -> if b then m `setBit` i     else m) 0 (zip [0..] [b0, b1, b2, b3, b4, b5, b6])
                                msb = foldr (\(i, b) m -> if b then m `setBit` (i-7) else m) 0 (zip [7..] [b7])
                            in (lsb, msb)
           return (mkMask pulseValue, mkMask (not pulseValue))
       let writeThrough (lsb, msb) = send $ DigitalPortWrite curPort lsb msb
       -- make sure masks are pre computed, and clear the line
       fst setMask `seq` snd setMask `seq` fst resetMask `seq` snd resetMask `seq` writeThrough resetMask
       -- Wait before starting the pulse
       liftIO $ threadDelay dBefore
       -- Send the pulse
       writeThrough setMask
       liftIO $ threadDelay dAfter
       -- Finish the pulse
       writeThrough resetMask
       -- Do a final internal update to reflect the final value of the line
       liftIO $ modifyMVar_ bs $ \bst -> return bst{pinStates = M.insert p PinData{pinMode = OUTPUT, pinValue = Just (Left (not pulseValue))}(pinStates bst)}
{-# ANN pulseOut_hostTiming "HLint: ignore Use camelCase" #-}

-- | A /hostOnly/ version of pulse-in on a digital-pin. Use this function only for cases where the
-- precision required only matters for the host, not for the board. That is, due to the inherent
-- delays involved in Firmata communication, the timing will /not/ be accurate, and should not
-- be expected to work uniformly over different boards. Similar comments apply for 'pulseOut_hostTiming'
-- as well. See the function 'pulse' for a more accurate version.
pulseIn_hostTiming :: Pin -> Bool -> Maybe Int -> Arduino (Maybe Int)
pulseIn_hostTiming p v mbTo = case mbTo of
                    Nothing -> Just `fmap` measure
                    Just to -> timeOut to measure
  where waitTill f = do curVal <- digitalRead p
                        unless (f curVal) $ waitTill f
        measure = do waitTill (== v)                  -- wait until pulse starts
                     (t, _) <- time $ waitTill (/= v) -- wait till pulse ends, measuring the time
                     return $ fromIntegral t
{-# ANN pulseIn_hostTiming "HLint: ignore Use camelCase" #-}

-- | Read the value of a pin in analog mode; this is a non-blocking call, immediately
-- returning the last sampled value. It returns @0@ if the voltage on the pin
-- is 0V, and @1023@ if it is 5V, properly scaled. (See `setAnalogSamplingInterval` for
-- sampling frequency.)
analogRead :: Pin -> Arduino Int
analogRead p' = do
   (_, pd) <- convertAndCheckPin "analogRead" p' ANALOG
   return $ case pinValue pd of
              Just (Right v) -> v
              _              -> 0 -- no (correctly-typed) value reported yet, default to 0

-- | Set the analog sampling interval, in milliseconds. Arduino uses a default of 19ms to sample analog and I2C
-- signals, which is fine for many applications, but can be modified if needed. The argument
-- should be a number between @10@ and @16384@; @10@ being the minumum sampling interval supported by Arduino
-- and @16383@ being the largest value we can represent in 14 bits that this message can handle. (Note that
-- the largest value is just about @16@ seconds, which is plenty infrequent for all practical needs.)
setAnalogSamplingInterval :: Int -> Arduino ()
setAnalogSamplingInterval i
  | i < 10 || i > 16383
  = die ("hArduino: setAnalogSamplingInterval: Allowed interval is [10, 16383] ms, received: " ++ show i) []
  | True
  = send $ SamplingInterval (fromIntegral lsb) (fromIntegral msb)
  where lsb = i .&. 0x7f
        msb = (i `shiftR` 7) .&. 0x7f
