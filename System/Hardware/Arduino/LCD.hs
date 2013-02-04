-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.LCD
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- LCD (Liquid Crystal Display) parts supported by hArduino
-------------------------------------------------------------------------------
module System.Hardware.Arduino.LCD(registerLCD, writeLCD)  where

import System.Hardware.Arduino.Data

import Control.Concurrent  (modifyMVar)
import Control.Monad.State (gets, liftIO)

import qualified Data.Map as M

-- | Register an LCD controller
registerLCD :: LCDController -> Arduino LCD
registerLCD controller = do
  bs <- gets boardState
  liftIO $ modifyMVar bs $ \bst -> do
             let n = M.size $ lcds bst
             return (bst {lcds = M.insert (LCD n) controller (lcds bst)}, LCD n)

-- | Write a string on an LCD
writeLCD :: LCD -> String -> Arduino ()
writeLCD _ m = liftIO $ putStrLn $ "TODO: This would go to the LCD: " ++ show m
