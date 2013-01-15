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

module System.Hardware.Arduino.Protocol where

import System.Hardware.Arduino.Parts

import qualified Data.ByteString as B
import Data.Bits
import Data.Char (chr, isAscii, isAlphaNum, isSpace)
import Data.List (intercalate)
import Data.Word
import Numeric   (showHex, showIntAtBase)

data Request = QueryFirmware
             | SetPinMode      Pin Mode
             | DigitalRead     Pin
             | DigitalPortWrite Int Word8 Word8

showBin :: (Integral a, Show a) => a -> String
showBin n = showIntAtBase 2 (head . show) n ""

instance Show Request where
   show QueryFirmware            = "QueryFirmWare"
   show (SetPinMode p m)         = "Set mode " ++ show p ++ " to " ++ show m
   show (DigitalRead p)          = "DigitalRead " ++ show p
   show (DigitalPortWrite p l m) = "Set port " ++ show p ++ " to " ++ showBin l ++ "_" ++ showBin m

data Response = Firmware  Word8 Word8 String
              | DigitalPinState Pin Mode Bool
              | DigitalPortState Int Word16
              | Unknown [Word8]

instance Show Response where
  show (Firmware majV minV n)  = "Firmware v" ++ show majV ++ "." ++ show minV ++ " (" ++ n ++ ")"
  show (DigitalPinState p m v) = "DigitalPinState " ++ show p ++ "(" ++ show m ++ ") = " ++ if v then "HIGH" else "LOW"
  show (DigitalPortState p w)  = "DigitalPortState " ++ show p ++ " = " ++ show w
  show (Unknown bs)            = "Unknown [" ++ intercalate ", " (map showByte bs) ++ "]"

cdStart, cdEnd :: Word8
cdStart = 0xf0
cdEnd   = 0xf7

sysEx :: [Word8] -> B.ByteString
sysEx bs = B.pack $ cdStart : bs ++ [cdEnd]

package :: Request -> B.ByteString
package QueryFirmware            = sysEx  [0x79]
package (SetPinMode p m)         = B.pack [0xf4, pinVal p, fromIntegral (fromEnum m)]
package (DigitalRead p)          = sysEx  [0x6d, pinVal p]
package (DigitalPortWrite p l m) = B.pack [0x90 .|. fromIntegral p, l, m]

unpackage :: B.ByteString -> Response
unpackage inp
  | length bs < 2 || head bs /= cdStart || last bs /= cdEnd
  = Unknown bs
  | True
  = getResponse (init (tail bs))
  where bs = B.unpack inp

getResponse :: [Word8] -> Response
getResponse (rf : majV : minV : rest)
  | rf == 0x79
  = Firmware majV minV (getString rest)
getResponse (pr : pinNo : pinMode : pinState : [])
  | pr == 0x6e
  = DigitalPinState (pin (fromIntegral pinNo)) (toEnum (fromIntegral pinMode)) (pinState /= 0)
getResponse (dpr : vL : vH : [])
  | dpr .&. 0xF0 == 0x90
  = let port = fromIntegral (dpr .&. 0x0F)
        w    = fromIntegral ((vH .&. 0x7F) `shiftL` 8) .|. fromIntegral (vL .&. 0x7F)
    in DigitalPortState port w
getResponse bs = Unknown bs

showByte :: Word8 -> String
showByte i | isVisible = [c]
           | i <= 0xf  = '0' : showHex i ""
           | True      = showHex i ""
  where c = chr $ fromIntegral i
        isVisible = isAscii c && isAlphaNum c && isSpace c

getString :: [Word8] -> String
getString []         = ""
getString [a]        = [chr (fromIntegral a)]  -- shouldn't happen, but no need to error out either
getString (l:h:rest) = c : getString rest
  where c = chr $ fromIntegral $ h `shiftL` 8 .|. l
