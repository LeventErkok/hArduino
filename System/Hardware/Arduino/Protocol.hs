-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Protocol
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Internal representation of the firmata protocol.
-------------------------------------------------------------------------------

module System.Hardware.Arduino.Protocol(Request(..), Response(..), package, unpackage) where

import Data.Bits ((.|.), (.&.), shiftL)
import Data.Char (chr)
import Data.List (intercalate)
import Data.Word (Word8, Word16)

import qualified Data.ByteString as B

import System.Hardware.Arduino.Parts
import System.Hardware.Arduino.Utils

-- | A request, as sent to Arduino
data Request = QueryFirmware                      -- ^ Query the Firmata version installed
             | SetPinMode       Pin PinMode       -- ^ Set a pin to a particular mode
             | DigitalRead      Pin               -- ^ Read the value of a given pin
             | DigitalReport    Int Bool          -- ^ Digital Report values on port enable/disable
             | DigitalPortWrite Int Word8 Word8   -- ^ Write the values of pins on the given port; 2 bytes lo/hi

instance Show Request where
   show QueryFirmware            = "QueryFirmWare"
   show (SetPinMode p m)         = "SetPinMode "   ++ show p ++ " to " ++ show m
   show (DigitalRead p)          = "DigitalRead "  ++ show p
   show (DigitalReport p b)      = "DigitalReport "  ++ show p ++ (if b then " enabled" else " disabled")
   show (DigitalPortWrite p l h) = "DigitalWrite " ++ show p ++ " to " ++ showBin l ++ "_" ++ showBin h

-- | A response, as returned from the Arduino
data Response = Firmware  Word8 Word8 String       -- ^ Firmware version (maj/min and indentifier
              | DigitalPinState Pin PinMode Bool   -- ^ State of a given pin
              | DigitalPortState Int Word16        -- ^ State of a given port
              | Unknown [Word8]                    -- ^ Represents messages currently unsupported

instance Show Response where
  show (Firmware majV minV n)  = "Firmware v" ++ show majV ++ "." ++ show minV ++ " (" ++ n ++ ")"
  show (DigitalPinState p m v) = "DigitalPinState " ++ show p ++ "(" ++ show m ++ ") = " ++ if v then "HIGH" else "LOW"
  show (DigitalPortState p w)  = "DigitalPortState " ++ show p ++ " = " ++ show w
  show (Unknown bs)            = "Unknown [" ++ intercalate ", " (map showByte bs) ++ "]"

-- | Marker for the start of a sys-ex message
cdStart :: Word8
cdStart = 0xf0

-- | Marker for the end of a sys-ex message
cdEnd :: Word8
cdEnd   = 0xf7

-- | Wrap a sys-ex message to be sent to the board
sysEx :: [Word8] -> B.ByteString
sysEx bs = B.pack $ cdStart : bs ++ [cdEnd]

-- | Package a request as a sequence of bytes to be sent to the board
-- using the Firmata protocol.
package :: Request -> B.ByteString
package QueryFirmware            = sysEx  [0x79]
package (SetPinMode p m)         = B.pack [0xf4, fromIntegral (pinNo p), fromIntegral (fromEnum m)]
package (DigitalRead p)          = sysEx  [0x6d, fromIntegral (pinNo p)]
package (DigitalReport p b)      = B.pack [0xd0 .|. fromIntegral p, if b then 1 else 0]
package (DigitalPortWrite p l m) = B.pack [0x90 .|. fromIntegral p, l, m]

-- | Unpackage a series of bytes as received from the board into a Response
unpackage :: B.ByteString -> Response
unpackage inp
  | length bs < 2 || head bs /= cdStart || last bs /= cdEnd   -- not sysex
  = getResponse bs
  | True        -- sysex; strip the markers
  = getResponse (init (tail bs))
  where bs = B.unpack inp

-- | Parse a response message. TBD: Use a proper (cereal based?) parser.
getResponse :: [Word8] -> Response
getResponse (rf : majV : minV : rest)
  | rf == 0x79
  = Firmware majV minV (getString rest)
getResponse (pr : curPin : pinMode : pinState : [])
  | pr == 0x6e
  = DigitalPinState (pin (fromIntegral curPin)) (toEnum (fromIntegral pinMode)) (pinState /= 0)
getResponse (dpr : vL : vH : [])
  | dpr .&. 0xF0 == 0x90
  = let port = fromIntegral (dpr .&. 0x0F)
        w    = fromIntegral ((vH .&. 0x7F) `shiftL` 8) .|. fromIntegral (vL .&. 0x7F)
    in DigitalPortState port w
getResponse bs = Unknown bs

-- | Turn a lo/hi encoded Arduino string constant into a Haskell string
getString :: [Word8] -> String
getString []         = ""
getString [a]        = [chr (fromIntegral a)]  -- shouldn't happen, but no need to error out either
getString (l:h:rest) = c : getString rest
  where c = chr $ fromIntegral $ h `shiftL` 8 .|. l
