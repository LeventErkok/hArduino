-- The mandatory "blink" example on Arduino

module System.Hardware.Arduino.Examples.Blink where

import Control.Monad           (forever, void)
import System.Hardware.Arduino

-- threadDelay is broken on Mac!
--   see: http://hackage.haskell.org/trac/ghc/ticket/7299
-- so use system/sleep, oh well.
import System.Process (system)

sleep :: Int -> IO ()
sleep n = void $ system $ "sleep " ++ show n

-- NB1. Make sure your Arduino is connected to a USB port
--      Replace the path to the USB device below accordingly
-- NB2. Make sure to "upload" the Firmata (v2.3) on Arduino
--      before running this program.
--
-- If things don't work, try changing the 'False' below
-- to 'True' in the call to 'withArduino', which will spit
-- out a bunch of debug messages. Hopefully, it'll be useful.
blink :: IO ()
blink = withArduino False "/dev/cu.usbmodemfd131" go
  where led = pin 13
        go arduino = do
                setPinMode arduino led OUTPUT
                forever $ do putStr "."
                             digitalWrite arduino led True
                             sleep 1
                             digitalWrite arduino led False
                             sleep 1
