-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Parts.Piezo
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Abstractions for piezo speakers. 
-------------------------------------------------------------------------------------------------

module System.Hardware.Arduino.Parts.Piezo(
   -- * Declaring a piezo speaker
     Piezo, speaker
   -- * Notes you can play, and durations
   , Note(..), Duration(..)
   -- * Playing a note, rest, or silencing
   , playNote, rest, silence
   -- * Play a sequence of notes:
   , playNotes
   ) where

import Data.Bits  (shiftR, (.&.))
import Data.Maybe (fromMaybe)

import System.Hardware.Arduino
import System.Hardware.Arduino.Comm
import System.Hardware.Arduino.Data

-- | A piezo speaker. Note that this type is abstract, use 'speaker' to
-- create an instance.
data Piezo = Piezo { piezoPin :: IPin  -- ^ The internal-pin that controls the speaker
                   , tempo    :: Int   -- ^ Tempo for the melody
                   }

-- | Create a piezo speaker instance.
speaker :: Int            -- ^ Tempo. Higher numbers mean faster melodies; in general.
        -> Pin            -- ^ Pin controlling the piezo. Should be a pin that supports PWM mode.
        -> Arduino Piezo
speaker t p = do debug $ "Attaching speaker on pin: " ++ show p
                 setPinMode p PWM
                 (ip, _) <- convertAndCheckPin "Piezo.speaker" p PWM
                 return Piezo { piezoPin = ip, tempo = t }

-- | Musical notes, notes around middle-C
data Note     = A | B | C | D | E | F | G | R  deriving (Eq, Show)  -- R is for rest

-- | Beat counts
data Duration = Whole | Half | Quarter | Eight deriving (Eq, Show)

-- | Convert a note to its frequency appropriate for Piezo
frequency :: Note -> Int
frequency n = fromMaybe 0 (n `lookup` fs)
 where fs = [(A, 440), (B, 493), (C, 261), (D, 294), (E, 329), (F, 349), (G, 392), (R, 0)]

-- | Convert a duration to a delay amount
interval :: Piezo -> Duration -> Int
interval p Whole   = 8 * interval p Eight
interval p Half    = 4 * interval p Eight
interval p Quarter = 2 * interval p Eight
interval p Eight   = tempo p

-- | Turn the speaker off
silence :: Piezo -> Arduino ()
silence (Piezo p _) = send $ AnalogPinWrite p 0 0

-- | Keep playing a given note on the piezo:
setNote :: Piezo -> Note -> Arduino ()
setNote (Piezo p _) n = send $ AnalogPinWrite p (fromIntegral lsb) (fromIntegral msb)
   where f   = frequency n
         lsb = f .&. 0x7f
         msb = (f `shiftR` 7) .&. 0x7f

-- | Play the given note for the duration
playNote :: Piezo -> (Note, Duration) -> Arduino ()
playNote pz (n, d) = do setNote pz n
                        delay (interval pz d)
                        silence pz

-- | Play a sequence of notes with given durations:
playNotes :: Piezo -> [(Note, Duration)] -> Arduino ()
playNotes pz = go
  where go []            = silence pz
        go (nd@(_, d):r) = do playNote pz nd
                              delay (interval pz d `div` 3) -- heuristically found.. :-)
                              go r

-- | Rest for a given duration:
rest :: Piezo -> Duration -> Arduino ()
rest pz d = delay (interval pz d)
