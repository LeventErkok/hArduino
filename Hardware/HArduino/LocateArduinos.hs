module Hardware.HArduino.LocateArduinos(locateArduinos) where

import Control.Monad (when)

import Hardware.HArduino.Data
import Hardware.HArduino.KnownBoards

import qualified System.USB  as USB
import qualified Data.Vector as V

locateArduinos :: Bool -> IO [Arduino]
locateArduinos debug = do
    ctx   <- USB.newCtx
    when debug $ USB.setDebug ctx USB.PrintInfo
    devs  <- V.toList `fmap` USB.getDevices ctx
    descs <- mapM USB.getDeviceDesc devs
    let dds = zip devs descs
    return [ Arduino b dev desc ctx Nothing
           | (dev, desc) <- dds, b <- knownBoards, matchDevice desc b]
 where matchDevice :: USB.DeviceDesc -> Board -> Bool
       matchDevice desc b =  deviceVendorId b  == USB.deviceVendorId desc
                          && deviceProductId b == USB.deviceProductId desc
