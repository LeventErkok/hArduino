-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Data
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Underlying data structures
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE NamedFieldPuns              #-}
module System.Hardware.Arduino.Data where

import Control.Applicative        (Applicative)
import Control.Concurrent         (Chan, MVar, modifyMVar, modifyMVar_, withMVar)
import Control.Monad.State        (StateT, MonadIO, MonadState, gets, liftIO)
import Data.Bits                  ((.&.), (.|.), setBit)
import Data.List                  (intercalate)
import Data.Maybe                 (fromMaybe)
import Data.Word                  (Word8)
import System.Hardware.Serialport (SerialPort)

import qualified Data.Map as M
import qualified Data.Set as S

import System.Hardware.Arduino.Utils

-- | A port (containing 8 pins)
data Port = Port { portNo :: Word8  -- ^ The port number
                 }
                 deriving (Eq, Ord)

instance Show Port where
  show p = "Port" ++ show (portNo p)

-- | A pin on the Arduino
data Pin = Pin { pinNo :: Word8   -- ^ The pin number
               }
         deriving (Eq, Ord)

instance Show Pin where
  show p | i < 10 = "Pin0" ++ show i
         | True   = "Pin"  ++ show i
   where i = pinNo p

-- | Declare a pin on the board by its number.
pin :: Word8 -> Pin
pin = Pin

-- | On the Arduino, pins are grouped into banks of 8.
-- Given a pin, this function determines which port it belongs to
pinPort :: Pin -> Port
pinPort p = Port (pinNo p `quot` 8)

-- | On the Arduino, pins are grouped into banks of 8.
-- Given a pin, this function determines which index it belongs to in its port
pinPortIndex :: Pin -> Word8
pinPortIndex p = pinNo p `rem` 8

-- | The mode for a pin.
data PinMode = INPUT    -- ^ Digital input
             | OUTPUT   -- ^ Digital output
             | ANALOG   -- ^ Analog input
             | PWM      -- ^ PWM (Pulse-Width-Modulation) output 
             | SERVO    -- ^ Servo Motor controller
             | SHIFT    -- ^ Shift controller
             | I2C      -- ^ I2C (Inter-Integrated-Circuit) connection
             deriving (Eq, Show, Enum)

-- | A request, as sent to Arduino
data Request = SystemReset                          -- ^ Send system reset
             | QueryFirmware                        -- ^ Query the Firmata version installed
             | CapabilityQuery                      -- ^ Query the capabilities of the board
             | AnalogMappingQuery                   -- ^ Query the mapping of analog pins
             | SetPinMode         Pin  PinMode      -- ^ Set the mode on a pin
             | DigitalReport      Port Bool         -- ^ Digital report values on port enable/disable
             | AnalogReport       Pin  Bool         -- ^ Analog report values on pin enable/disable
             | DigitalPortWrite   Port Word8 Word8  -- ^ Set the values on a port digitally
             | SamplingInterval   Word8 Word8       -- ^ Set the sampling interval
             deriving Show

-- | A response, as returned from the Arduino
data Response = Firmware  Word8 Word8 String         -- ^ Firmware version (maj/min and indentifier
              | Capabilities BoardCapabilities       -- ^ Capabilities report
              | AnalogMapping [Word8]                -- ^ Analog pin mappings
              | DigitalMessage Port Word8 Word8      -- ^ Status of a port
              | AnalogMessage  Pin  Word8 Word8      -- ^ Status of an analog pin
              | Unimplemented (Maybe String) [Word8] -- ^ Represents messages currently unsupported

instance Show Response where
  show (Firmware majV minV n)  = "Firmware v" ++ show majV ++ "." ++ show minV ++ " (" ++ n ++ ")"
  show (Capabilities b)        = "Capabilities:\n" ++ show b
  show (AnalogMapping bs)      = "AnalogMapping: " ++ showByteList bs
  show (DigitalMessage p l h)  = "DigitalMessage " ++ show p ++ " = " ++ showByte l ++ " " ++ showByte h
  show (AnalogMessage  p l h)  = "AnalogMessage "  ++ show p ++ " = " ++ showByte l ++ " " ++ showByte h
  show (Unimplemented mbc bs)  = "Unimplemeneted " ++ fromMaybe "" mbc ++ " " ++ showByteList bs

-- | Resolution, as referred to in http://firmata.org/wiki/Protocol#Capability_Query
-- TODO: Not quite sure how this is used, so merely keep it as a Word8 now
type Resolution = Word8

-- | Capabilities of a pin
type PinCapabilities  = ( Maybe Word8               -- Analog pin number, if any
                        , [(PinMode, Resolution)]
                        )

-- | What the board is capable of and current settings
newtype BoardCapabilities = BoardCapabilities (M.Map Pin PinCapabilities)

instance Show BoardCapabilities where
  show (BoardCapabilities m) = intercalate "\n" (map sh (M.toAscList m))
    where sh (p, (mbA, pc)) = show p ++ sep ++ unwords [show md | (md, _) <- pc]
             where sep = maybe ": " (\i -> "[A" ++ show i ++ "]: ") mbA

-- | Data associated with a pin
data PinData = PinData {
                 pinMode  :: PinMode
               , pinValue :: Maybe (Either Bool Int)
               }
               deriving Show

-- | LCD's connected to the board
newtype LCD = LCD Int
            deriving (Eq, Ord, Show)

-- | Hitachi LCD controller: See: <http://en.wikipedia.org/wiki/Hitachi_HD44780_LCD_controller>.
-- We model only the 4-bit variant, with RS and EN lines only. (The most common Arduino usage.)
-- The data sheet can be seen at: <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>.
data LCDController = Hitachi44780 {
                       lcdRS       :: Pin  -- ^ Hitachi pin @ 4@: Register-select
                     , lcdEN       :: Pin  -- ^ Hitachi pin @ 6@: Enable
                     , lcdD4       :: Pin  -- ^ Hitachi pin @11@: Data line @4@
                     , lcdD5       :: Pin  -- ^ Hitachi pin @12@: Data line @5@
                     , lcdD6       :: Pin  -- ^ Hitachi pin @13@: Data line @6@
                     , lcdD7       :: Pin  -- ^ Hitachi pin @14@: Data line @7@
                     , lcdRows     :: Int  -- ^ Number of rows (typically 1 or 2, upto 4)
                     , lcdCols     :: Int  -- ^ Number of cols (typically 16 or 20, upto 40)
                     , dotMode5x10 :: Bool -- ^ Set to True if 5x10 dots are used
                     }
                     deriving Show

-- | State of the LCD, a mere 8-bit word for the Hitachi
data LCDData = LCDData {
                  lcdDisplayMode    :: Word8         -- ^ Display mode (left/right/scrolling etc.)
                , lcdDisplayControl :: Word8         -- ^ Display control (blink on/off, display on/off etc.)
                , lcdGlyphCount     :: Word8         -- ^ Count of custom created glyphs (typically at most 8)
                , lcdController     :: LCDController -- ^ Actual controller
                }

-- | State of the board
data BoardState = BoardState {
                    boardCapabilities    :: BoardCapabilities  -- ^ Capabilities of the board
                  , analogReportingPins  :: S.Set Pin          -- ^ Which analog pins are reporting
                  , digitalReportingPins :: S.Set Pin          -- ^ Which digital pins are reporting
                  , pinStates            :: M.Map Pin PinData  -- ^ For-each pin, store its data
                  , digitalWakeUpQueue   :: [MVar ()]          -- ^ Semaphore list to wake-up upon receiving a digital message
                  , lcds                 :: M.Map LCD LCDData  -- ^ LCD's attached to the board
                  }

-- | State of the computation
data ArduinoState = ArduinoState {
                message       :: String -> IO ()     -- ^ Current debugging routine
              , port          :: SerialPort          -- ^ Serial port we are communicating on
              , firmataID     :: String              -- ^ The ID of the board (as identified by the Board itself)
              , boardState    :: MVar BoardState     -- ^ Current state of the board
              , deviceChannel :: Chan Response       -- ^ Incoming messages from the board
              , capabilities  :: BoardCapabilities   -- ^ Capabilities of the board
              }

-- | The Arduino monad.
newtype Arduino a = Arduino (StateT ArduinoState IO a)
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState ArduinoState)

-- | Debugging only: print the given string on stdout.
debug :: String -> Arduino ()
debug s = do f <- gets message
             liftIO $ f s

-- | Which modes does this pin support?
getPinModes :: Pin -> Arduino [PinMode]
getPinModes p = do
  BoardCapabilities caps <- gets capabilities
  case p `M.lookup` caps of
    Nothing      -> return []
    Just (_, ps) -> return (map fst ps)

-- | Current state of the pin
getPinData :: Pin -> Arduino PinData
getPinData p = do
  bs  <- gets boardState
  liftIO $ withMVar bs $ \bst ->
     case p `M.lookup` pinStates bst of
       Nothing -> die ("Trying to access " ++ show p ++ " without proper configuration.")
                      ["Make sure that you use 'setPinMode' to configure this pin first."]
       Just pd -> return pd

-- | Given a pin, collect the digital value corresponding to the
-- port it belongs to, where the new value of the current pin is given
-- The result is two bytes:
--
--   * First  lsb: pins 0-6 on the port
--   * Second msb: pins 7-13 on the port
--
-- In particular, the result is suitable to be sent with a digital message
computePortData :: Pin -> Bool -> Arduino (Word8, Word8)
computePortData curPin newValue = do
  let curPort  = pinPort curPin
  let curIndex = pinPortIndex curPin
  bs <- gets boardState
  liftIO $ modifyMVar bs $ \bst -> do
     let values = [(pinPortIndex p, pinValue pd) | (p, pd) <- M.assocs (pinStates bst), curPort == pinPort p, pinMode pd `elem` [INPUT, OUTPUT]]
         getVal i
           | i == curIndex                             = newValue
           | Just (Just (Left v)) <- i `lookup` values = v
           | True                                      = False
         [b0, b1, b2, b3, b4, b5, b6, b7] = map getVal [0 .. 7]
         lsb = foldr (\(i, b) m -> if b then m `setBit` i     else m) 0 (zip [0..] [b0, b1, b2, b3, b4, b5, b6])
         msb = foldr (\(i, b) m -> if b then m `setBit` (i-7) else m) 0 (zip [7..] [b7])
         bst' = bst{pinStates = M.insert curPin PinData{pinMode = OUTPUT, pinValue = Just (Left newValue)}(pinStates bst)}
     return (bst', (lsb, msb))

-- | Keep track of listeners on a digital message
digitalWakeUp :: MVar () -> Arduino ()
digitalWakeUp semaphore = do
    bs <- gets boardState
    liftIO $ modifyMVar_ bs $ \bst -> return bst{digitalWakeUpQueue = semaphore : digitalWakeUpQueue bst}

-- | Firmata commands, see: http://firmata.org/wiki/Protocol#Message_Types
data FirmataCmd = ANALOG_MESSAGE      Pin  -- ^ @0xE0@ pin
                | DIGITAL_MESSAGE     Port -- ^ @0x90@ port
                | REPORT_ANALOG_PIN   Pin  -- ^ @0xC0@ pin
                | REPORT_DIGITAL_PORT Port -- ^ @0xD0@ port
                | START_SYSEX              -- ^ @0xF0@
                | SET_PIN_MODE             -- ^ @0xF4@
                | END_SYSEX                -- ^ @0xF7@
                | PROTOCOL_VERSION         -- ^ @0xF9@
                | SYSTEM_RESET             -- ^ @0xFF@
                deriving Show

-- | Compute the numeric value of a command
firmataCmdVal :: FirmataCmd -> Word8
firmataCmdVal (ANALOG_MESSAGE      p) = 0xE0 .|. pinNo  p
firmataCmdVal (DIGITAL_MESSAGE     p) = 0x90 .|. portNo p
firmataCmdVal (REPORT_ANALOG_PIN   p) = 0xC0 .|. pinNo  p
firmataCmdVal (REPORT_DIGITAL_PORT p) = 0xD0 .|. portNo p
firmataCmdVal START_SYSEX             = 0xF0
firmataCmdVal SET_PIN_MODE            = 0xF4
firmataCmdVal END_SYSEX               = 0xF7
firmataCmdVal PROTOCOL_VERSION        = 0xF9
firmataCmdVal SYSTEM_RESET            = 0xFF

-- | Convert a byte to a Firmata command
getFirmataCmd :: Word8 -> Either Word8 FirmataCmd
getFirmataCmd w = classify
  where extract m | w .&. m == m = Just $ fromIntegral (w .&. 0x0F)
                  | True         = Nothing
        classify | w == 0xF0              = Right START_SYSEX
                 | w == 0xF4              = Right SET_PIN_MODE
                 | w == 0xF7              = Right END_SYSEX
                 | w == 0xF9              = Right PROTOCOL_VERSION
                 | w == 0xFF              = Right SYSTEM_RESET
                 | Just i <- extract 0xE0 = Right $ ANALOG_MESSAGE      (Pin i)
                 | Just i <- extract 0x90 = Right $ DIGITAL_MESSAGE     (Port i)
                 | Just i <- extract 0xC0 = Right $ REPORT_ANALOG_PIN   (Pin i)
                 | Just i <- extract 0xD0 = Right $ REPORT_DIGITAL_PORT (Port i)
                 | True                   = Left w

-- | Sys-ex commands, see: http://firmata.org/wiki/Protocol#Sysex_Message_Format
data SysExCmd = RESERVED_COMMAND        -- ^ @0x00@  2nd SysEx data byte is a chip-specific command (AVR, PIC, TI, etc).
              | ANALOG_MAPPING_QUERY    -- ^ @0x69@  ask for mapping of analog to pin numbers
              | ANALOG_MAPPING_RESPONSE -- ^ @0x6A@  reply with mapping info
              | CAPABILITY_QUERY        -- ^ @0x6B@  ask for supported modes and resolution of all pins
              | CAPABILITY_RESPONSE     -- ^ @0x6C@  reply with supported modes and resolution
              | PIN_STATE_QUERY         -- ^ @0x6D@  ask for a pin's current mode and value
              | PIN_STATE_RESPONSE      -- ^ @0x6E@  reply with a pin's current mode and value
              | EXTENDED_ANALOG         -- ^ @0x6F@  analog write (PWM, Servo, etc) to any pin
              | SERVO_CONFIG            -- ^ @0x70@  set max angle, minPulse, maxPulse, freq
              | STRING_DATA             -- ^ @0x71@  a string message with 14-bits per char
              | SHIFT_DATA              -- ^ @0x75@  shiftOut config/data message (34 bits)
              | I2C_REQUEST             -- ^ @0x76@  I2C request messages from a host to an I/O board
              | I2C_REPLY               -- ^ @0x77@  I2C reply messages from an I/O board to a host
              | I2C_CONFIG              -- ^ @0x78@  Configure special I2C settings such as power pins and delay times
              | REPORT_FIRMWARE         -- ^ @0x79@  report name and version of the firmware
              | SAMPLING_INTERVAL       -- ^ @0x7A@  sampling interval
              | SYSEX_NON_REALTIME      -- ^ @0x7E@  MIDI Reserved for non-realtime messages
              | SYSEX_REALTIME          -- ^ @0x7F@  MIDI Reserved for realtime messages
              deriving Show

-- | Convert a 'SysExCmd' to a byte
sysExCmdVal :: SysExCmd -> Word8
sysExCmdVal RESERVED_COMMAND        = 0x00
sysExCmdVal ANALOG_MAPPING_QUERY    = 0x69
sysExCmdVal ANALOG_MAPPING_RESPONSE = 0x6A
sysExCmdVal CAPABILITY_QUERY        = 0x6B
sysExCmdVal CAPABILITY_RESPONSE     = 0x6C
sysExCmdVal PIN_STATE_QUERY         = 0x6D
sysExCmdVal PIN_STATE_RESPONSE      = 0x6E
sysExCmdVal EXTENDED_ANALOG         = 0x6F
sysExCmdVal SERVO_CONFIG            = 0x70
sysExCmdVal STRING_DATA             = 0x71
sysExCmdVal SHIFT_DATA              = 0x75
sysExCmdVal I2C_REQUEST             = 0x76
sysExCmdVal I2C_REPLY               = 0x77
sysExCmdVal I2C_CONFIG              = 0x78
sysExCmdVal REPORT_FIRMWARE         = 0x79
sysExCmdVal SAMPLING_INTERVAL       = 0x7A
sysExCmdVal SYSEX_NON_REALTIME      = 0x7E
sysExCmdVal SYSEX_REALTIME          = 0x7F

-- | Convert a byte into a 'SysExCmd'
getSysExCommand :: Word8 -> Either Word8 SysExCmd
getSysExCommand 0x00 = Right RESERVED_COMMAND
getSysExCommand 0x69 = Right ANALOG_MAPPING_QUERY
getSysExCommand 0x6A = Right ANALOG_MAPPING_RESPONSE
getSysExCommand 0x6B = Right CAPABILITY_QUERY
getSysExCommand 0x6C = Right CAPABILITY_RESPONSE
getSysExCommand 0x6D = Right PIN_STATE_QUERY
getSysExCommand 0x6E = Right PIN_STATE_RESPONSE
getSysExCommand 0x6F = Right EXTENDED_ANALOG
getSysExCommand 0x70 = Right SERVO_CONFIG
getSysExCommand 0x71 = Right STRING_DATA
getSysExCommand 0x75 = Right SHIFT_DATA
getSysExCommand 0x76 = Right I2C_REQUEST
getSysExCommand 0x77 = Right I2C_REPLY
getSysExCommand 0x78 = Right I2C_CONFIG
getSysExCommand 0x79 = Right REPORT_FIRMWARE
getSysExCommand 0x7A = Right SAMPLING_INTERVAL
getSysExCommand 0x7E = Right SYSEX_NON_REALTIME
getSysExCommand 0x7F = Right SYSEX_REALTIME
getSysExCommand n    = Left n

-- | Keep track of pin-mode changes
registerPinMode :: Pin -> PinMode -> Arduino [Request]
registerPinMode p m = do
        -- first check that the requested mode is supported for this pin
        BoardCapabilities caps <- gets capabilities
        case p `M.lookup` caps of
          Nothing
             -> die ("Invalid access to unsupported pin: " ++ show p)
                    ("Available pins are: " : ["  " ++ show k | (k, _) <- M.toAscList caps])
          Just (_, ms)
            | m `notElem` map fst ms
            -> die ("Invalid mode " ++ show m ++ " set for " ++ show p)
                   ["Supported modes for this pin are: " ++ unwords (if null ms then ["NONE"] else map show ms)]
          _ -> return ()
        -- see if there was a mode already set for this pin
        bs  <- gets boardState
        mbOldMode <- liftIO $ withMVar bs $ \bst ->
                                case p `M.lookup` pinStates bst of
                                  Nothing -> return Nothing -- completely new, register
                                  Just pd -> return $ Just $ pinMode pd
        -- depending on old/new mode, determine what actions to take
        let registerNewMode = modifyMVar_ bs $ \bst -> return bst{pinStates = M.insert p PinData{pinMode = m, pinValue = Nothing} (pinStates bst) }
        case mbOldMode of
          Nothing -> do liftIO registerNewMode
                        getModeActions p m
          Just m' | m == m' -> return []  -- no mode change, nothing to do
                  | True    -> do liftIO registerNewMode
                                  remActs <- getRemovalActions p m'
                                  addActs <- getModeActions p m
                                  return $ remActs ++ addActs

-- | A mode was removed from this pin, update internal state and determine any necessary actions to remove it
getRemovalActions :: Pin -> PinMode -> Arduino [Request]
getRemovalActions p INPUT  = do -- This pin is no longer digital input
        bs <- gets boardState
        liftIO $ modifyMVar bs $ \bst -> do
                let dPins = p `S.delete` digitalReportingPins bst
                    port  = pinPort p
                    acts  = [DigitalReport port False | port `notElem` map pinPort (S.elems dPins)]   -- no need for a digital report on this port anymore
                    bst'  = bst { digitalReportingPins = dPins }
                return (bst', acts)
getRemovalActions p ANALOG = do -- This pin is no longer analog
        bs <- gets boardState
        liftIO $ modifyMVar bs $ \bst -> do
                let aPins = analogReportingPins bst
                    acts  = [AnalogReport p False | p `S.member` aPins] -- no need for an analog report on this port anymore
                    bst'  = bst { analogReportingPins = p `S.delete` aPins }
                return (bst', acts)
getRemovalActions _ OUTPUT = return []
getRemovalActions p m = error $ "hArduino: getRemovalActions: TBD: Unsupported mode: " ++ show m ++ " on pin " ++ show p

-- | Depending on a mode-set call, determine what further
-- actions should be executed, such as enabling/disabling pin/port reporting
getModeActions :: Pin -> PinMode -> Arduino [Request]
getModeActions p INPUT  = do -- This pin is just configured for digital input
        bs <- gets boardState
        liftIO $ modifyMVar bs $ \bst -> do
                    let aPins = analogReportingPins bst
                        dPins = digitalReportingPins bst
                        port  = pinPort p
                        acts1 = [AnalogReport  p    False | p    `S.member` aPins]                       -- there was an analog report, remove it
                        acts2 = [DigitalReport port True  | port `notElem`  map pinPort (S.elems dPins)] -- there was no digital report, add it
                        bst' = bst { analogReportingPins  = p `S.delete` analogReportingPins  bst
                                   , digitalReportingPins = p `S.insert` digitalReportingPins bst
                                   }
                    return (bst', acts1 ++ acts2)
getModeActions p ANALOG = do -- This pin just configured for analog
        bs <- gets boardState
        liftIO $ modifyMVar bs $ \bst -> do
                    let aPins = analogReportingPins bst
                        dPins = p `S.delete` digitalReportingPins bst
                        port  = pinPort p
                        acts1 = [AnalogReport  p    True  | p    `S.notMember` aPins]                       -- there was no analog report, add it
                        acts2 = [DigitalReport port False | port `notElem`     map pinPort (S.elems dPins)] -- no need for a digital report, remove it
                        bst' = bst { analogReportingPins  = p `S.insert` analogReportingPins  bst
                                   , digitalReportingPins = dPins
                                   }
                    return (bst', acts1 ++ acts2)
getModeActions _ OUTPUT = return []
getModeActions p m      = error $ "hArduino: getModeActions: TBD: Unsupported mode: " ++ show m ++ " on pin " ++ show p
