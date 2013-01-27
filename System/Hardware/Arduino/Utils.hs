-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Utils
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Internal utilities
-------------------------------------------------------------------------------
module System.Hardware.Arduino.Utils where

import Control.Concurrent (threadDelay)
import Control.Monad      (void)
import Data.Bits          ((.|.), shiftL)
import Data.Char          (isAlphaNum, isAscii, isSpace, chr)
import Data.IORef         (newIORef, readIORef, writeIORef)
import Data.List          (intercalate)
import Data.Word          (Word8)
import System.Process     (system)
import System.Info        (os)
import Numeric            (showHex, showIntAtBase, showFFloat)

-- | Delay (wait) for the given number of milli-seconds
-- NB. The 'threadDelay' function is broken on Mac, see <http://hackage.haskell.org/trac/ghc/ticket/7299>
-- Until there's a new GHC release that fixes this issue, we temporarily use system/sleep on Mac.
delay :: Int -> IO ()
delay n
  | os == "darwin"
  = void $ system $ "sleep " ++ showFFloat (Just 2) (fromIntegral n / (1000 :: Double)) ""
  | True
  = threadDelay (n*1000)

-- | A simple printer that can keep track of sequence numbers. Used for debugging purposes.
mkDebugPrinter :: Bool -> IO (String -> IO ())
mkDebugPrinter False = return (const (return ()))
mkDebugPrinter True  = do
        cnt <- newIORef (1::Int)
        let f s = do i <- readIORef cnt
                     writeIORef cnt (i+1)
                     putStrLn $ "[" ++ show i ++ "] hArduino: " ++ s
        return f

-- | Show a byte in a visible format.
showByte :: Word8 -> String
showByte i | isVisible = [c]
           | i <= 0xf  = '0' : showHex i ""
           | True      = showHex i ""
  where c = chr $ fromIntegral i
        isVisible = isAscii c && isAlphaNum c && isSpace c

-- | Show a list of bytes
showByteList :: [Word8] -> String
showByteList bs =  "[" ++ intercalate ", " (map showByte bs) ++ "]"

-- | Show a number as a binary value
showBin :: (Integral a, Show a) => a -> String
showBin n = showIntAtBase 2 (head . show) n ""

-- | Turn a lo/hi encoded Arduino string constant into a Haskell string
getString :: [Word8] -> String
getString []         = ""
getString [a]        = [chr (fromIntegral a)]  -- shouldn't happen, but no need to error out either
getString (l:h:rest) = c : getString rest
  where c = chr $ fromIntegral $ h `shiftL` 8 .|. l

-- | Error out
die :: String -> [String] -> a
die m ms = error $ "\n*** hArduino:ERROR: " ++ intercalate "\n*** " (m:ms)
