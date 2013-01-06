module System.Hardware.Arduino.Utils where

import Data.IORef

mkDebugPrinter :: Bool -> IO (String -> IO ())
mkDebugPrinter False = return (const (return ()))
mkDebugPrinter True  = do
        cnt <- newIORef (1::Int)
        let f s = do i <- readIORef cnt
                     writeIORef cnt (i+1)
                     putStrLn $ "[" ++ show i ++ "] hArduino: " ++ s
        return f
