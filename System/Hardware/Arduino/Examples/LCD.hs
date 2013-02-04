-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Examples.LCD
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Basic demo of an Hitachi HD44780 LCD
-------------------------------------------------------------------------------

module System.Hardware.Arduino.Examples.LCD where

import Control.Monad.Trans (liftIO)

import System.Hardware.Arduino

-- | Access the LCD connected to Arduino, making it show messages
-- we read from the user.
lcdDemo :: IO ()
lcdDemo = withArduino False "/dev/cu.usbmodemfd131" $ do
              lcd <- registerLCD Hitachi44780
              go lcd
  where go lcd = repl
         where repl = do liftIO $ putStr "Message> "
                         m <- liftIO getLine
                         case m of
                           ":q" -> return ()
                           _    -> do writeLCD lcd m
                                      repl
