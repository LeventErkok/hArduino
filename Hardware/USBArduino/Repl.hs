{-# LANGUAGE NamedFieldPuns #-}
module Hardware.USBArduino.Repl(repl) where

import Control.Monad (unless)
import System.IO
import qualified System.USB as USB

import Hardware.USBArduino.Data

repl :: (Board, USB.Device, USB.DeviceDesc) -> IO ()
repl (b@Board{replPrompt}, _, _) = do
        putStrLn $ "Using board " ++ show b ++ "."
        hSetBuffering stdout NoBuffering
        go (replPrompt ++ "> ")
 where go p = do putStr p
                 c <- getLine
                 q <- process c
                 unless q $ go p

process :: String -> IO Bool
process ":q" = return True
process _    = do putStrLn "Unknown command."
                  return False
