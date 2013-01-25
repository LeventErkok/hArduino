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
import Control.Concurrent         (Chan, MVar)
import Control.Monad.State        (StateT, MonadIO, MonadState, gets, liftIO)
import Data.Bits                  ((.&.), (.|.))
import Data.List                  (intercalate)
import Data.Word                  (Word8, Word16)
import System.Hardware.Serialport (SerialPort)

import System.Hardware.Arduino.Utils
import System.Hardware.Arduino.Parts hiding (pin)

-- | A request, as sent to Arduino
data Request = QueryFirmware                      -- ^ Query the Firmata version installed
             | CapabilityQuery                    -- ^ Query the capabilities of the board
             | SetPinMode       Pin PinMode       -- ^ Set a pin to a particular mode
             | DigitalRead      Pin               -- ^ Read the value of a given pin
             | DigitalReport    Word8 Bool        -- ^ Digital Report values on port enable/disable
             | DigitalPortWrite Word8 Word8 Word8 -- ^ Write the values of pins on the given port; 2 bytes lo/hi

instance Show Request where
   show QueryFirmware            = "QueryFirmWare"
   show CapabilityQuery          = "CapabilityQuery"
   show (SetPinMode p m)         = "SetPinMode "   ++ show p ++ " to " ++ show m
   show (DigitalRead p)          = "DigitalRead "  ++ show p
   show (DigitalReport p b)      = "DigitalReport "  ++ show p ++ (if b then " enabled" else " disabled")
   show (DigitalPortWrite p l h) = "DigitalWrite " ++ show p ++ " to " ++ showBin l ++ "_" ++ showBin h

-- | A response, as returned from the Arduino
data Response = Firmware  Word8 Word8 String       -- ^ Firmware version (maj/min and indentifier
              | DigitalPinState Pin PinMode Bool   -- ^ State of a given pin
              | DigitalPortState Int Word16        -- ^ State of a given port
              | Capabilities BoardCapabilities     -- ^ Capabilities report
              | Unknown [Word8]                    -- ^ Represents messages currently unsupported

instance Show Response where
  show (Firmware majV minV n)  = "Firmware v" ++ show majV ++ "." ++ show minV ++ " (" ++ n ++ ")"
  show (DigitalPinState p m v) = "DigitalPinState " ++ show p ++ "(" ++ show m ++ ") = " ++ if v then "HIGH" else "LOW"
  show (DigitalPortState p w)  = "DigitalPortState " ++ show p ++ " = " ++ show w
  show (Capabilities b)        = "Capabilities " ++ show b
  show (Unknown bs)            = "Unknown [" ++ intercalate ", " (map showByte bs) ++ "]"


-- | What the board is capable of and current settings
type BoardCapabilities = ()

-- | State of the board
data BoardState = BoardState {
                   capabilities :: BoardCapabilities
                }

-- | State of the computation
data ArduinoState = ArduinoState {
                message       :: String -> IO ()     -- ^ Current debugging routine
              , port          :: SerialPort          -- ^ Serial port we are communicating on
              , firmataID     :: String    -- ^ The ID of the board (as identified by the Board itself)
              , boardState    :: MVar BoardState     -- ^ Current state of the board
              , deviceChannel :: Chan Response       -- ^ Incoming messages from the board
              }

-- | The Arduino monad.
newtype Arduino a = Arduino (StateT ArduinoState IO a)
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState ArduinoState)

-- | Debugging only: print the given string on stdout.
debug :: String -> Arduino ()
debug s = do f <- gets message
             liftIO $ f s

-- | Firmata commands, see: http://firmata.org/wiki/Protocol#Message_Types
data FirmataCmd = ANALOG_MESSAGE      Word8 -- ^ @0xE0@ pin
                | DIGITAL_MESSAGE     Word8 -- ^ @0x90@ port
                | REPORT_ANALOG_PIN   Word8 -- ^ @0xC0@ pin
                | REPORT_DIGITAL_PORT Word8 -- ^ @0xD0@ port
                | START_SYSEX               -- ^ @0xF0@
                | SET_PIN_MODE              -- ^ @0xF4@
                | END_SYSEX                 -- ^ @0xF7@
                | PROTOCOL_VERSION          -- ^ @0xF9@
                | SYSTEM_RESET              -- ^ @0xFF@
                deriving Show

-- | Compute the numeric value of a command
firmataCmdVal :: FirmataCmd -> Word8
firmataCmdVal (ANALOG_MESSAGE      pin ) = 0xE0 .|. pin
firmataCmdVal (DIGITAL_MESSAGE     port) = 0x90 .|. port
firmataCmdVal (REPORT_ANALOG_PIN   pin ) = 0xC0 .|. pin
firmataCmdVal (REPORT_DIGITAL_PORT port) = 0xD0 .|. port
firmataCmdVal START_SYSEX                = 0xF0
firmataCmdVal SET_PIN_MODE               = 0xF4
firmataCmdVal END_SYSEX                  = 0xF7
firmataCmdVal PROTOCOL_VERSION           = 0xF9
firmataCmdVal SYSTEM_RESET               = 0xFF

-- | Convert a byte to a Firmata command
getFirmataCmd :: Word8 -> FirmataCmd
getFirmataCmd w = classify
  where extract m | w .&. m == m = Just $ fromIntegral (w .&. 0x0F)
                  | True         = Nothing
        classify | w == 0xF0              = START_SYSEX
                 | w == 0xF4              = SET_PIN_MODE
                 | w == 0xF7              = END_SYSEX
                 | w == 0xF9              = PROTOCOL_VERSION
                 | w == 0xFF              = SYSTEM_RESET
                 | Just i <- extract 0xE0 = ANALOG_MESSAGE      i
                 | Just i <- extract 0x90 = DIGITAL_MESSAGE     i
                 | Just i <- extract 0xC0 = REPORT_ANALOG_PIN   i
                 | Just i <- extract 0xD0 = REPORT_DIGITAL_PORT i
                 | True                   = error $ "hArduino: Received unknown command word: " ++ showByte w

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
getSysExCommand :: Word8 -> SysExCmd
getSysExCommand 0x00 = RESERVED_COMMAND
getSysExCommand 0x69 = ANALOG_MAPPING_QUERY
getSysExCommand 0x6A = ANALOG_MAPPING_RESPONSE
getSysExCommand 0x6B = CAPABILITY_QUERY
getSysExCommand 0x6C = CAPABILITY_RESPONSE
getSysExCommand 0x6D = PIN_STATE_QUERY
getSysExCommand 0x6E = PIN_STATE_RESPONSE
getSysExCommand 0x6F = EXTENDED_ANALOG
getSysExCommand 0x70 = SERVO_CONFIG
getSysExCommand 0x71 = STRING_DATA
getSysExCommand 0x75 = SHIFT_DATA
getSysExCommand 0x76 = I2C_REQUEST
getSysExCommand 0x77 = I2C_REPLY
getSysExCommand 0x78 = I2C_CONFIG
getSysExCommand 0x79 = REPORT_FIRMWARE
getSysExCommand 0x7A = SAMPLING_INTERVAL
getSysExCommand 0x7E = SYSEX_NON_REALTIME
getSysExCommand 0x7F = SYSEX_REALTIME
getSysExCommand n    = error $ "hArduino: Received unknown sysex command word: " ++ showByte n
