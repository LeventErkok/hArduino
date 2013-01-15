-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Data
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Underlying data structures
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE NamedFieldPuns              #-}
module System.Hardware.Arduino.Data where

import Control.Applicative              (Applicative)
import Control.Monad.State              (StateT, MonadIO, MonadState, gets, liftIO)
import Data.Maybe                       (fromMaybe)
import System.Hardware.Serialport       (SerialPort)

import System.Hardware.Arduino.Protocol

-- | Basic communication channel with the board.
data ArduinoChannel = ArduinoChannel {
                  recvChan  :: IO Response              -- ^ Receive a sys-ex response.
                , recvNChan :: Int -> IO Response       -- ^ Receive a non-sys-ex response that has /n/ bytes in it
                , sendChan  :: Request -> IO ()         -- ^ Send a request down to Arduino
                }

-- | State of the board and other machinery.
data ArduinoState = ArduinoState {
                message       :: String -> IO ()        -- ^ Current debugging routine
              , port          :: SerialPort             -- ^ Port we are communicating on
              , firmataID     :: String                 -- ^ The ID of the board (as identified by the Board itself)
              , deviceChannel :: Maybe ArduinoChannel   -- ^ Communication channel
              }

-- | The Arduino monad.
newtype Arduino a = Arduino (StateT ArduinoState IO a)
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState ArduinoState)

-- | Retrieve the current channel.
getChannel :: Arduino ArduinoChannel
getChannel = fromMaybe die `fmap` gets deviceChannel
  where die = error "Cannot communicate with the board!"

-- | Send down a request.
send :: Request -> Arduino ()
send r = do ArduinoChannel{sendChan} <- getChannel
            liftIO $ sendChan r

-- | Receive a sys-ex response.
recv :: Arduino Response
recv = do ArduinoChannel{recvChan} <- getChannel
          liftIO recvChan

-- | Receive a non-sys-ex response, that has /n/ bytes
recvN :: Int -> Arduino Response
recvN n = do ArduinoChannel{recvNChan} <- getChannel
             liftIO $ recvNChan n

-- | Debugging only: print it on stdout.
debug :: String -> Arduino ()
debug s = do f <- gets message
             liftIO $ f s
