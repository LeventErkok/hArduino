module Hardware.HArduino.LocateArduinos(locateArduinos) where

import Hardware.HArduino.Data
import Hardware.HArduino.KnownBoards

import qualified System.USB  as USB
import qualified Data.Vector as V

locateArduinos :: IO [Arduino]
locateArduinos = do ctx   <- USB.newCtx
                    devs  <- V.toList `fmap` USB.getDevices ctx
                    descs <- mapM USB.getDeviceDesc devs
                    let dds = zip devs descs
                    return [Arduino b dev desc ctx | (dev, desc) <- dds, b <- knownBoards, matchDevice desc b]
 where matchDevice :: USB.DeviceDesc -> Board -> Bool
       matchDevice desc b =  deviceVendorId b  == USB.deviceVendorId desc
                          && deviceProductId b == USB.deviceProductId desc
