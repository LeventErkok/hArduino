-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.LCD
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- LCD (Liquid Crystal Display) parts supported by hArduino. The Haskell code
-- below has partly been coded by following the Arduino LiquidCrystal project
-- source code: <http://code.google.com/p/arduino/source/browse/trunk/libraries/LiquidCrystal/>
--
-- The Hitachi44780 data sheet is at: http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.LCD(registerLCD, LCDDisplayProperties(..), setLCDProperties, clearLCD, writeLCD)  where

import Control.Concurrent  (modifyMVar, withMVar)
import Control.Monad.State (gets, liftIO)
import Data.Bits           (testBit, (.|.))
import Data.Char           (ord)
import Data.Word           (Word8)

import qualified Data.Map as M

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Firmata

---------------------------------------------------------------------------------------
-- High level interface, exposed to the user
---------------------------------------------------------------------------------------

-- | Register an LCD controller
registerLCD :: LCDController -> Arduino LCD
registerLCD controller = do
  bs <- gets boardState
  lcd <- liftIO $ modifyMVar bs $ \bst -> do
                    let n = M.size $ lcds bst
                    return (bst {lcds = M.insert (LCD n) (0, controller) (lcds bst)}, LCD n)
  case controller of
     Hitachi44780{} -> initLCD lcd controller
  return lcd

-- | Write a string on an LCD
writeLCD :: LCD -> String -> Arduino ()
writeLCD lcd m = do
   c <- getController lcd
   let cvt ch = fromIntegral (ord ch) .|. 0xFF
   mapM_ (sendData c . cvt) m

---------------------------------------------------------------------------------------
-- Low level interface, not available to the user
---------------------------------------------------------------------------------------

-- | Entry modes
data EntryModes = LCD_ENTRYLEFT
                | LCD_ENTRYRIGHT
                | LCD_ENTRYSHIFTINCREMENT
                | LCD_ENTRYSHIFTDECREMENT

-- | Convert entry mode to val.
getModeVal :: EntryModes -> Word8
getModeVal LCD_ENTRYLEFT           = 0x02
getModeVal LCD_ENTRYRIGHT          = 0x00
getModeVal LCD_ENTRYSHIFTINCREMENT = 0x01
getModeVal LCD_ENTRYSHIFTDECREMENT = 0x00

-- | Commands understood by Hitachi
data Cmd = LCD_INITIALIZE
         | LCD_INITIALIZE_END
         | LCD_FUNCTIONSET
         | LCD_DISPLAYCONTROL Word8
         | LCD_CLEARDISPLAY
         | LCD_ENTRYMODESET [EntryModes]

-- | Convert a command to a data-word
getCmdVal :: LCDController -> Cmd -> Word8
getCmdVal Hitachi44780{lcdRows, dotMode5x10} = get
  where multiLine -- bit 3
          | lcdRows > 1 = 0x08 :: Word8
          | True        = 0x00 :: Word8
        dotMode   -- bit 2
          | dotMode5x10 = 0x04 :: Word8
          | True        = 0x00 :: Word8
        displayFunction = multiLine .|. dotMode
        get LCD_INITIALIZE         = 0x03
        get LCD_INITIALIZE_END     = 0x02
        get LCD_FUNCTIONSET        = 0x20 .|. displayFunction
        get (LCD_DISPLAYCONTROL w) = 0x08 .|. w
        get LCD_CLEARDISPLAY       = 0x01
        get (LCD_ENTRYMODESET ms)  = 0x04 .|. foldr ((.|.) . getModeVal) 0 ms

-- | Display properties
data LCDDisplayProperties = LCD_DISPLAYON  -- ^ Turn the display on
                          | LCD_DISPLAYOFF -- ^ Turn the display off
                          | LCD_CURSORON   -- ^ Turn the cursor on
                          | LCD_CURSOROFF  -- ^ Turn the cursor off
                          | LCD_BLINKON    -- ^ Start blinking the cursor
                          | LCD_BLINKOFF   -- ^ Stop blinking the cursor

getPropVal :: Word8 -> [LCDDisplayProperties] -> Word8
getPropVal = foldr ((.|.) . get)
  where get LCD_DISPLAYON  = 0x04
        get LCD_DISPLAYOFF = 0x00
        get LCD_CURSORON   = 0x02
        get LCD_CURSOROFF  = 0x00
        get LCD_BLINKON    = 0x01
        get LCD_BLINKOFF   = 0x00

-- | Initialize the LCD. Follows the data sheet <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>,
-- page 46; figure 24.
initLCD :: LCD -> LCDController -> Arduino ()
initLCD lcd c@Hitachi44780{lcdRS, lcdEN, lcdD4, lcdD5, lcdD6, lcdD7} = do
    mapM_ (`setPinMode` OUTPUT) [lcdRS, lcdEN, lcdD4, lcdD5, lcdD6, lcdD7]
    -- Wait for 50ms, data-sheet says at least 40ms for 2.7V version, so be safe
    delay 50
    -- According to the flow-chart on that page we need to send 0x3 three times, with proper delay
    sequence_ $ concat $ replicate 3 [sendCmd c LCD_INITIALIZE, delay 1]
    sendCmd c LCD_INITIALIZE_END
    sendCmd c LCD_FUNCTIONSET
    setLCDProperties lcd [LCD_DISPLAYON, LCD_CURSOROFF, LCD_BLINKOFF]
    clearLCD lcd
    sendCmd c (LCD_ENTRYMODESET [LCD_ENTRYLEFT, LCD_ENTRYSHIFTDECREMENT])

-- | Set display properties
setLCDProperties :: LCD -> [LCDDisplayProperties] -> Arduino ()
setLCDProperties lcd props = do
  bs <- gets boardState
  (c, displayProps) <- liftIO $ modifyMVar bs $ \bst ->
                          case lcd `M.lookup` lcds bst of
                            Nothing            -> error $ "hArduino: Cannot locate " ++ show lcd
                            Just (curProps, c) -> do let newProps = getPropVal curProps props
                                                     return (bst {lcds = M.insert lcd (newProps, c) (lcds bst)}, (c, newProps))
  sendCmd c (LCD_DISPLAYCONTROL displayProps)

-- | Get the controller associated with the LCD
getController :: LCD -> Arduino LCDController
getController lcd = do
  bs <- gets boardState
  liftIO $ withMVar bs $ \bst -> case lcd `M.lookup` lcds bst of
                                   Nothing     -> error $ "hArduino: Cannot locate " ++ show lcd
                                   Just (_, c) -> return c

-- | Clear the LCD
clearLCD :: LCD -> Arduino ()
clearLCD lcd = do
   c <- getController lcd
   sendCmd c LCD_CLEARDISPLAY
   delay 2 -- give some time to make sure LCD is really cleared

-- | Send a command to the LCD controller
sendCmd :: LCDController -> Cmd -> Arduino ()
sendCmd c = transmit False c . getCmdVal c

-- | Send 4-bit data to the LCD controller
sendData :: LCDController -> Word8 -> Arduino ()
sendData = transmit True

-- | By controlling the enable-pin, indicate to the controller that
-- the data is ready for it to process.
pulseEnable :: LCDController -> Arduino ()
pulseEnable Hitachi44780{lcdEN} = do
  digitalWrite lcdEN False
  delay 1
  digitalWrite lcdEN True
  delay 1
  digitalWrite lcdEN False
  delay 1

-- | Transmit data down to the LCD
transmit :: Bool -> LCDController -> Word8 -> Arduino ()
transmit mode c@Hitachi44780{lcdRS, lcdEN, lcdD4, lcdD5, lcdD6, lcdD7} val = do
  digitalWrite lcdRS mode
  digitalWrite lcdEN False
  let [b7, b6, b5, b4, b3, b2, b1, b0] = [val `testBit` i | i <- [7, 6 .. 0]]
  -- Send down the first 4 bits
  digitalWrite lcdD4 b0
  digitalWrite lcdD5 b1
  digitalWrite lcdD6 b2
  digitalWrite lcdD7 b3
  -- Signal data sent
  pulseEnable c
  -- Send down the second 4 bits
  digitalWrite lcdD4 b4
  digitalWrite lcdD5 b5
  digitalWrite lcdD6 b6
  digitalWrite lcdD7 b7
