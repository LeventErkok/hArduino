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

-- | Connections for a basic hitachi controller
-- See <http://en.wikipedia.org/wiki/Hitachi_HD44780_LCD_controller> for
-- pin layout. For this demo, simply connect the LCD pins to the Arduino
-- as follows:
--
--  * LCD pin @01@ to GND
--
--  * LCD pin @02@ to +5V
--
--  * LCD pin @03@ to a 10K potentiometer's viper
--
--  * LCD pin @04@ to Arduino pin @12@
--
--  * LCD pin @05@ to GND
--
--  * LCD pin @06@ to Arduino pin @11@
--
--  * LCD pin @11@ to Arduino pin @5@
--
--  * LCD pin @12@ to Arduino pin @4@
--
--  * LCD pin @13@ to Arduino pin @3@
--
--  * LCD pin @14@ to Arduino pin @2@
--
--  * [If backlight is needed] LCD pin @15@ to +5V
--
--  * [If backlight is needed] LCD pin @16@ to GND via 220ohm resistor
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/Examples/Schematics/LCD.png>>
hitachi :: LCDController
-- Connections:                   ARDUINO     Hitachi   Description
--------------------------------  -------    ---------  ----------------
hitachi = Hitachi44780 { lcdRS   = pin 12  --     4      Register-select
                       , lcdEN   = pin 11  --     6      Enable
                       , lcdD4   = pin  5  --    11      Data 4
                       , lcdD5   = pin  4  --    12      Data 5
                       , lcdD6   = pin  3  --    13      Data 6
                       , lcdD7   = pin  2  --    14      Data 7
                       -- Other config variables for the display
                       , lcdRows     = 2    -- 2 rows
                       , lcdCols     = 16    -- of 16 columns
                       , dotMode5x10 = False -- Using the standard 5x8 dots
                       }

-- | Access the LCD connected to Arduino, making it show messages
-- we read from the user.
lcdDemo :: IO ()
lcdDemo = withArduino True "/dev/cu.usbmodemfd131" $ do
              lcd <- registerLCD hitachi
              liftIO $ putStrLn "Hitachi controller demo.. Type :q to quit."
              let repl = do liftIO $ putStr "Message> "
                            m <- liftIO getLine
                            case m of
                             ":q" -> return ()
                             _    -> do clearLCD lcd
                                        writeLCD lcd m
                                        repl
              repl
