module Main(main) where

import Hardware.HArduino.LocateArduinos
import Hardware.HArduino.Repl

main :: IO ()
main = do bs <- locateArduinos
          case bs of
           []  -> return ()
           [b] -> repl b
           _   -> putStrLn "Don't know how to work with multiple devices yet.."
