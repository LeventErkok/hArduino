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
module System.Hardware.Arduino.LCD(
             lcdRegister
           , lcdClear, lcdWrite
           , lcdHome, lcdSetCursor
           , lcdDisplayOff, lcdDisplayOn
           , lcdCursorOn, lcdCursorOff , lcdNoBlink, lcdBlink
           , lcdLeftToRight, lcdRightToLeft
           , lcdScrollDisplayLeft, lcdScrollDisplayRight
           , lcdAutoScrollOff, lcdAutoScrollOn
        )  where

import Control.Concurrent  (modifyMVar, withMVar)
import Control.Monad.State (gets, liftIO)
import Data.Bits           (testBit, (.|.), (.&.))
import Data.Char           (ord)
import Data.Maybe          (fromMaybe)
import Data.Word           (Word8)

import qualified Data.Map as M

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Firmata

import qualified System.Hardware.Arduino.Utils as U

---------------------------------------------------------------------------------------
-- Low level interface, not available to the user
---------------------------------------------------------------------------------------

-- | Entry modes
data EntryModes = LCD_ENTRYLEFT
                | LCD_ENTRYRIGHT
                | LCD_ENTRYSHIFTINCREMENT
                | LCD_ENTRYSHIFTDECREMENT

-- | Convert entry mode to val
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
         | LCD_RETURNHOME
         | LCD_SETDDRAMADDR Word8

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
        get LCD_INITIALIZE         = 0x33
        get LCD_INITIALIZE_END     = 0x32
        get LCD_FUNCTIONSET        = 0x20 .|. displayFunction
        get (LCD_DISPLAYCONTROL w) = 0x08 .|. w
        get LCD_CLEARDISPLAY       = 0x01
        get (LCD_ENTRYMODESET ms)  = 0x04 .|. foldr ((.|.) . getModeVal) 0 ms
        get LCD_RETURNHOME         = 0x02
        get (LCD_SETDDRAMADDR w)   = 0x80 .|. w

-- | Display properties
data LCDDisplayProperties = LCD_DISPLAYON  -- ^ Turn the display on
                          | LCD_DISPLAYOFF -- ^ Turn the display off
                          | LCD_CURSORON   -- ^ Turn the cursor on
                          | LCD_CURSOROFF  -- ^ Turn the cursor off
                          | LCD_BLINKON    -- ^ Start blinking the cursor
                          | LCD_BLINKOFF   -- ^ Stop blinking the cursor
                          deriving Show

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
    debug "Starting the LCD initialization sequence"
    mapM_ (`setPinMode` OUTPUT) [lcdRS, lcdEN, lcdD4, lcdD5, lcdD6, lcdD7]
    -- Wait for 50ms, data-sheet says at least 40ms for 2.7V version, so be safe
    delay 50
    sendCmd c LCD_INITIALIZE
    delay 5
    sendCmd c LCD_INITIALIZE_END
    sendCmd c LCD_FUNCTIONSET
    setLCDProperties lcd [LCD_DISPLAYON, LCD_CURSOROFF, LCD_BLINKOFF]
    lcdClear lcd
    sendCmd c (LCD_ENTRYMODESET [LCD_ENTRYLEFT, LCD_ENTRYSHIFTDECREMENT])

-- | Set display properties
setLCDProperties :: LCD -> [LCDDisplayProperties] -> Arduino ()
setLCDProperties lcd props = do
  debug $ "Setting LCD properties: " ++ show props
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

-- | Send a command to the LCD controller
sendCmd :: LCDController -> Cmd -> Arduino ()
sendCmd c = transmit False c . getCmdVal c

-- | Send 4-bit data to the LCD controller
sendData :: LCDController -> Word8 -> Arduino ()
sendData lcd n = do debug $ "Transmitting LCD data: " ++ U.showByte n
                    transmit True lcd n

-- | By controlling the enable-pin, indicate to the controller that
-- the data is ready for it to process.
pulseEnable :: LCDController -> Arduino ()
pulseEnable Hitachi44780{lcdEN} = do
  debug "Sending LCD pulseEnable"
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
  digitalWrite lcdD4 b4
  digitalWrite lcdD5 b5
  digitalWrite lcdD6 b6
  digitalWrite lcdD7 b7
  pulseEnable c
  -- Send down the remaining batch
  digitalWrite lcdD4 b0
  digitalWrite lcdD5 b1
  digitalWrite lcdD6 b2
  digitalWrite lcdD7 b3
  pulseEnable c

-- | Helper function to simplify library programming, not exposed to the user.
withLCD :: LCD -> String -> (LCDController -> Arduino a) -> Arduino a
withLCD lcd what action = do
        debug what
        c <- getController lcd
        action c

---------------------------------------------------------------------------------------
-- High level interface, exposed to the user
---------------------------------------------------------------------------------------

-- | Register an LCD controller
lcdRegister :: LCDController -> Arduino LCD
lcdRegister controller = do
  bs <- gets boardState
  lcd <- liftIO $ modifyMVar bs $ \bst -> do
                    let n = M.size $ lcds bst
                    return (bst {lcds = M.insert (LCD n) (0, controller) (lcds bst)}, LCD n)
  case controller of
     Hitachi44780{} -> initLCD lcd controller
  return lcd

-- | Write a string on the LCD at the current cursor position
lcdWrite :: LCD -> String -> Arduino ()
lcdWrite lcd m = withLCD lcd ("Writing " ++ show m ++ " to LCD") $ \c -> mapM_ (sendData c) m'
   where m' = map (\ch -> fromIntegral (ord ch) .&. 0xFF) m

-- | Clear the LCD
lcdClear :: LCD -> Arduino ()
lcdClear lcd = withLCD lcd "Sending clearLCD" $ \c ->
                 do sendCmd c LCD_CLEARDISPLAY
                    delay 2 -- give some time to make sure LCD is really cleared

-- | Send the cursor to home position
lcdHome :: LCD -> Arduino ()
lcdHome lcd = withLCD lcd "Sending the cursor home" $ \c ->
                do sendCmd c LCD_RETURNHOME
                   delay 2

-- | Set the cursor location. The pair of arguments is the new column and row numbers
-- respectively:
--
--   * The first value is the column, the second is the row. (This is counter-intuitive, but
--     is in line with what the standard Arduino programmers do, so we follow the same convention.)
--
--   * Counting starts at 0 (both for column and row no)
--
--   * If the new location is out-of-bounds of your LCD, we will put it the cursor to the closest
--     possible location on the LCD.
lcdSetCursor :: LCD -> (Int, Int) -> Arduino ()
lcdSetCursor lcd (givenCol, givenRow) = withLCD lcd ("Sending the cursor to Row: " ++ show givenRow ++ " Col: " ++ show givenCol) set
  where set c@Hitachi44780{lcdRows, lcdCols} = sendCmd c (LCD_SETDDRAMADDR offset)
              where align :: Int -> Int -> Word8
                    align i m
                      | i < 0  = 0
                      | i >= m = fromIntegral $ m-1
                      | True   = fromIntegral i
                    col = align givenCol lcdCols
                    row = align givenRow lcdRows
                    -- The magic row-offsets come from various web sources
                    -- I don't follow the logic in these numbers, but it seems to work
                    rowOffsets = [(0, 0), (1, 0x40), (2, 0x14), (3, 0x54)]
                    offset = col + fromMaybe 0x54 (row `lookup` rowOffsets)

-- | Turn the display off
lcdDisplayOff :: LCD -> Arduino ()
lcdDisplayOff = error "TBD: needs display control"

-- | Turn the display on
lcdDisplayOn :: LCD -> Arduino ()
lcdDisplayOn = error "TBD: needs display control"

-- | Do not blink the cursor
lcdNoBlink :: LCD -> Arduino ()
lcdNoBlink = error "TBD: needs display control"

-- | Blink the cursor
lcdBlink :: LCD -> Arduino ()
lcdBlink = error "TBD: needs display control"

-- | Show the cursor
lcdCursorOn :: LCD -> Arduino ()
lcdCursorOn = error "TBD: needs display control"

-- | Hide the cursor
lcdCursorOff :: LCD -> Arduino ()
lcdCursorOff = error "TBD: needs display control"

-- | Scroll the display to the left by 1 character
lcdScrollDisplayLeft :: LCD -> Arduino ()
lcdScrollDisplayLeft = error "TBD"

-- | Scroll the display to the right by 1 character
lcdScrollDisplayRight :: LCD -> Arduino ()
lcdScrollDisplayRight = error "TBD"

-- | Set writing direction: Left to Right
lcdLeftToRight :: LCD -> Arduino ()
lcdLeftToRight = error "TBD: needs display mode"

-- | Set writing direction: Right to Left
lcdRightToLeft :: LCD -> Arduino ()
lcdRightToLeft = error "TBD: needs display mode"

-- | Turn on auto-scrolling
lcdAutoScrollOn :: LCD -> Arduino ()
lcdAutoScrollOn = error "TBD: needs display mode"

-- | Turn off auto-scrolling
lcdAutoScrollOff :: LCD -> Arduino ()
lcdAutoScrollOff = error "TBD: needs display mode"
