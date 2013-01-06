{-# LANGUAGE NamedFieldPuns #-}
module Hardware.HArduino.Comm where

import Data.Char   (ord)
import Data.Vector ((!))
import System.USB
import qualified Data.ByteString as B

import Hardware.HArduino.Data
import Hardware.HArduino.LocateArduinos

withArduino :: Bool -> (Arduino -> IO a) -> IO a
withArduino debug f = do as <- locateArduinos debug
                         case as of
                           [a] -> withChannel a
                           []  -> error "Cannot connect to Arduino."
                           _   -> error "Unsupported: Multiple Arduino's found."
 where withChannel arduino@Arduino{device} =
         withDeviceHandle device             $ \devHndl ->
         withDetachedKernelDriver devHndl 0  $ do
               config0 <- getConfigDesc device 0
               let interface0 = configInterfaces config0 ! 0
                   alternate0 = interface0 ! 0
                   endpoint1  = interfaceEndpoints alternate0 ! 0

               let recv nb to = do
                       (bs, status) <- readInterrupt devHndl
                                                     (endpointAddress endpoint1)
                                                     nb
                                                     to
                       case status of
                         TimedOut  -> return Nothing
                         Completed -> return $ Just $ concatMap show $ B.unpack bs
               let send str to = do
                       (_, status) <- writeInterrupt devHndl
                                                     (endpointAddress endpoint1)
                                                     (B.pack (map (fromIntegral . ord) str))
                                                     to
                       case status of
                         TimedOut  -> return False
                         Completed -> return True
               f $ arduino{deviceChannel = Just (ArduinoChannel recv send)}
