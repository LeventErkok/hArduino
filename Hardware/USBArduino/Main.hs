module Main(main) where

import Hardware.USBArduino.LocateArduinos

main :: IO ()
main = locateArduinos >>= print
