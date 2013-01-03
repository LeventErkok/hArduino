module Hardware.USBArduino.LocateArduinos(locateArduinos) where

import Hardware.USBArduino.Data
import Hardware.USBArduino.KnownBoards

import qualified System.USB  as USB
import qualified Data.Vector as V

locateArduinos :: IO [(Board, USB.Device, USB.DeviceDesc)]
locateArduinos = do ctx   <- USB.newCtx
                    devs  <- V.toList `fmap` USB.getDevices ctx
                    descs <- mapM USB.getDeviceDesc devs
                    let dds = zip devs descs
                    return [(b, device, desc) | (device, desc) <- dds, b <- knownBoards, matchDevice desc b]
 where matchDevice :: USB.DeviceDesc -> Board -> Bool
       matchDevice desc b =  deviceVendorId b  == USB.deviceVendorId desc
                          && deviceProductId b == USB.deviceProductId desc
