module System.Hardware.Arduino.Protocol where

import System.Hardware.Arduino.Parts

import qualified Data.ByteString as B
import Data.Bits
import Data.Char (chr)
import Data.List (intercalate)
import Data.Word
import Numeric   (showHex)

data Request = QueryFirmware
             | SetPinMode    Pin Mode
             | DigitalRead   Pin
             | DigitalWrite  Pin Bool

instance Show Request where
   show QueryFirmware      = "QueryFirmWare"
   show (SetPinMode p m)   = "Set mode " ++ show p ++ " to " ++ show m
   show (DigitalRead p)    = "DigitalRead " ++ show p
   show (DigitalWrite p b) = "Set pin " ++ show p ++ " to " ++ (if b then " HIGH" else " LOW")

data Response = Firmware Word8 Word8 String
              | PinValue Bool
              | Unknown [Word8]

instance Show Response where
  show (Firmware majV minV n) = "Firmware v" ++ show majV ++ "." ++ show minV ++ " (" ++ n ++ ")"
  show (PinValue v)           = "PinValue " ++ if v then "HIGH" else "LOW"
  show (Unknown bs)           = "Unknown [" ++ intercalate ", " (map showByte bs) ++ "]"

cdStart, cdEnd :: Word8
cdStart = 0xf0
cdEnd   = 0xf7

package :: Request -> B.ByteString
package QueryFirmware    = B.pack [cdStart, 0x79, cdEnd]
package (SetPinMode p m) = B.pack [0xf4, pinVal p, fromIntegral (fromEnum m)]
package (DigitalRead p)  = B.pack [0xd0 .|. pinVal p]
package (DigitalWrite{}) = error "TBD"

unpackage :: B.ByteString -> Response
unpackage inp
  | length bs < 2 || head bs /= cdStart || last bs /= cdEnd
  = Unknown bs
  | True
  = getResponse (init (tail bs))
  where bs = B.unpack inp

getResponse :: [Word8] -> Response
getResponse (rf : majV : minV : rest)
  | rf == 0x79 = Firmware majV minV (getString rest)
getResponse bs = Unknown bs

showByte :: Word8 -> String
showByte i | i <= 0xf = "0x0" ++ showHex i ""
           | True     = "0x"  ++ showHex i ""

getString :: [Word8] -> String
getString []         = ""
getString [a]        = [chr (fromIntegral a)]  -- shouldn't happen, but no need to error out either
getString (l:h:rest) = c : getString rest
  where c = chr $ fromIntegral $ h `shiftL` 8 .|. l
