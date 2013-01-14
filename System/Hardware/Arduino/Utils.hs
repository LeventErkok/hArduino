module System.Hardware.Arduino.Utils where

import Control.Monad (void)

import Data.IORef
-- threadDelay is broken on Mac!
-- see: http://hackage.haskell.org/trac/ghc/ticket/7299
-- so use system/sleep, oh well.
import System.Process (system)

sleep :: Int -> IO ()
sleep n = void $ system $ "sleep " ++ show n

mkDebugPrinter :: Bool -> IO (String -> IO ())
mkDebugPrinter False = return (const (return ()))
mkDebugPrinter True  = do
        cnt <- newIORef (1::Int)
        let f s = do i <- readIORef cnt
                     writeIORef cnt (i+1)
                     putStrLn $ "[" ++ show i ++ "] hArduino: " ++ s
        return f
