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

module System.Hardware.Arduino.Protocol(package, unpackageSysEx, unpackageNonSysEx) where

import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.Map        as M

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Utils

-- | Wrap a sys-ex message to be sent to the board
sysEx :: SysExCmd -> [Word8] -> B.ByteString
sysEx cmd bs = B.pack $  firmataCmdVal START_SYSEX
                      :  sysExCmdVal cmd
                      :  bs
                      ++ [firmataCmdVal END_SYSEX]

-- | Construct a non sys-ex message
nonSysEx :: FirmataCmd -> [Word8] -> B.ByteString
nonSysEx cmd bs = B.pack $ firmataCmdVal cmd : bs

-- | Package a request as a sequence of bytes to be sent to the board
-- using the Firmata protocol.
package :: Request -> B.ByteString
package QueryFirmware            = sysEx    REPORT_FIRMWARE         []
package CapabilityQuery          = sysEx    CAPABILITY_QUERY        []
package AnalogMappingQuery       = sysEx    ANALOG_MAPPING_QUERY    []
package (AnalogReport  p b)      = nonSysEx (REPORT_ANALOG_PIN p)   [if b then 1 else 0]
package (DigitalReport p b)      = nonSysEx (REPORT_DIGITAL_PORT p) [if b then 1 else 0]
package (SetPinMode p m)         = nonSysEx SET_PIN_MODE            [fromIntegral (pinNo p), fromIntegral (fromEnum m)]
{-
package (DigitalRead p)          = sysEx    PIN_STATE_QUERY         [fromIntegral (pinNo p)]
package (DigitalPortWrite p l m) = nonSysEx (DIGITAL_MESSAGE p)     [l, m]
-}

-- | Unpackage a SysEx response
unpackageSysEx :: [Word8] -> Response
unpackageSysEx []              = Unimplemented (Just "<EMPTY-SYSEX-CMD>") []
unpackageSysEx (cmdWord:args)
  | Right cmd <- getSysExCommand cmdWord
  = case (cmd, args) of
      (REPORT_FIRMWARE, majV : minV : rest) -> Firmware majV minV (getString rest)
      (CAPABILITY_RESPONSE, bs)             -> Capabilities (getCapabilities bs)
      (ANALOG_MAPPING_RESPONSE, bs)         -> AnalogMapping bs
      _                                     -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

getCapabilities :: [Word8] -> BoardCapabilities
getCapabilities bs = BoardCapabilities $ M.fromList $ zipWith (\p c -> (p, (Nothing, c))) (map pin [0..]) (map pinCaps (chunk bs))
  where chunk xs = case break (== 0x7f) xs of
                     ([], [])         -> []
                     (cur, 0x7f:rest) -> cur : chunk rest
                     _                -> [xs]
        pinCaps (x:y:rest) = (toEnum (fromIntegral x), y) : pinCaps rest
        pinCaps _          = []

-- | Unpackage a Non-SysEx response
unpackageNonSysEx :: (Int -> IO [Word8]) -> FirmataCmd -> IO Response
unpackageNonSysEx getBytes c = grab c
 where unimplemented n = Unimplemented (Just (show c)) `fmap` getBytes n
       grab (ANALOG_MESSAGE      _pin ) = unimplemented 2
       grab (DIGITAL_MESSAGE     _port) = unimplemented 2
       grab (REPORT_ANALOG_PIN   _pin ) = unimplemented 1
       grab (REPORT_DIGITAL_PORT _port) = unimplemented 1
       grab START_SYSEX                 = unimplemented 0   -- we should never see this
       grab SET_PIN_MODE                = unimplemented 2
       grab END_SYSEX                   = unimplemented 0   -- we should never see this
       grab PROTOCOL_VERSION            = unimplemented 2
       grab SYSTEM_RESET                = unimplemented 0
