module ProgramImage where

import Data.Word

data DCD = DCD [DCDCommand]

data DCDCommand =
  DCDWrite {
    byteWidth :: Int,
    -- (address, value) pairs
    associations :: [(Word32, Word32)]} |
  NOP
