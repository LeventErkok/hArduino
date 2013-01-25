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

module System.Hardware.Arduino.Protocol(Request(..), Response(..), package) where

import Data.Word (Word8)

import qualified Data.ByteString as B

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Parts

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
package (SetPinMode p m)         = nonSysEx SET_PIN_MODE            [fromIntegral (pinNo p), fromIntegral (fromEnum m)]
package (DigitalRead p)          = sysEx    PIN_STATE_QUERY         [fromIntegral (pinNo p)]
package (DigitalReport p b)      = nonSysEx (REPORT_DIGITAL_PORT p) [if b then 1 else 0]
package (DigitalPortWrite p l m) = nonSysEx (DIGITAL_MESSAGE p)     [l, m]
