{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE NamedFieldPuns              #-}
module System.Hardware.Arduino.Data where

import Control.Monad.State
import Data.Maybe
import System.Hardware.Serialport
import System.Hardware.Arduino.Protocol

data ArduinoChannel = ArduinoChannel {
                  recvChan  :: IO Response
                , recvNChan :: Int -> IO Response
                , sendChan  :: Request -> IO ()
                }

data Board = Board {
                message       :: String -> IO ()
              , port          :: SerialPort
              , firmataID     :: String
              , deviceChannel :: Maybe ArduinoChannel
              }

newtype Arduino a = Arduino (StateT Board IO a)
                  deriving (Functor, Monad, MonadIO, MonadState Board)

getChannel :: Arduino ArduinoChannel
getChannel = fromMaybe die `fmap` gets deviceChannel
  where die = error "Cannot communicate with the board!"

send :: Request -> Arduino ()
send r = do ArduinoChannel{sendChan} <- getChannel
            liftIO $ sendChan r

recv :: Arduino Response
recv = do ArduinoChannel{recvChan} <- getChannel
          liftIO recvChan

recvN :: Int -> Arduino Response
recvN n = do ArduinoChannel{recvNChan} <- getChannel
             liftIO $ recvNChan n

debug :: String -> Arduino ()
debug s = do f <- gets message
             liftIO $ f s
