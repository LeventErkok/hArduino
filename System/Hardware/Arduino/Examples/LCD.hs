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
              lcd <- registerLCD hitachi
              go lcd
  where -- Connections:                  ARDUINO      Hitachi    Description
        -------------------------------  -------   ------------ ----------------
        hitachi = Hitachi44780 { lcdRS   = pin 2   --    4       Register-select
                               , lcdEN   = pin 3   --    6       Enable
                               , lcdD4   = pin 4   --   11       Data 4
                               , lcdD5   = pin 5   --   12       Data 5
                               , lcdD6   = pin 6   --   13       Data 6
                               , lcdD7   = pin 7   --   14       Data 7
                               }
        go lcd = repl
         where repl = do liftIO $ putStr "Message> "
                         m <- liftIO getLine
                         case m of
                           ":q" -> return ()
                           _    -> do writeLCD lcd m
                                      repl
