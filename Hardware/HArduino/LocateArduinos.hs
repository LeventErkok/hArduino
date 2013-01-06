module Hardware.HArduino.LocateArduinos(locateArduinos) where

import Hardware.HArduino.Data
import Hardware.HArduino.KnownBoards

import qualified System.USB  as USB
import qualified Data.Vector as V

locateArduinos :: IO [(Board, USB.Device, USB.DeviceDesc)]
locateArduinos = do putStr "Searching serial devices..."
                    ctx   <- USB.newCtx
                    devs  <- V.toList `fmap` USB.getDevices ctx
                    putStr $ " Querying " ++ show (length devs) ++ " device(s)... "
                    descs <- mapM USB.getDeviceDesc devs
                    let dds = zip devs descs
                        r = [(b, device, desc) | (device, desc) <- dds, b <- knownBoards, matchDevice desc b]
                    case r of
                      []  -> putStrLn "No known Arduino devices found."
                      [_] -> putStrLn "Found one Arduino board attached."
                      _   -> putStrLn $ "Found " ++ show (length r) ++ " Arduino boards."
                    return r
 where matchDevice :: USB.DeviceDesc -> Board -> Bool
       matchDevice desc b =  deviceVendorId b  == USB.deviceVendorId desc
                          && deviceProductId b == USB.deviceProductId desc
