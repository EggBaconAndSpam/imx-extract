module ProgramImage where

import Data.Bits
import Data.Word

import Parsing
import qualified Structs

class FromStruct a where
  type StructType a
  fromStruct :: StructType a -> a

data DCDWriteMode = Write | MaskSet | MaskClear

data DCDCheckMode = CheckClear | CheckSet | CheckAnySet | CheckAnyClear

data DCDCommand =
  DCDWrite {
    byteWidth :: Int,
    writeMode :: DCDWriteMode,
    -- (address, value) pairs
    associations :: [(Word32, Word32)]} |
  DCDCheck {
    byteWidth :: Int,
    checkMode :: DCDCheckMode,
    checkAddress :: Word32,
    checkMask :: Word32,
    count :: Maybe Word32 } |
  DCDNOP |
  DCDUnlock  -- stub

instance FromStruct DCDCommand where
  type StructType DCDCommand = Structs.DCD_Command

  fromStruct (Structs.DCD_Command header (Structs.DCD_Write assocs)) =
    DCDWrite byteWidth writeMode assocs
   where
    params = Structs.header_parameter header

    byteWidth | testBit params 0 = 1
              | testBit params 1 = 2
              | testBit params 2 = 4

    writeMode | testBit params 3 && testBit params 4 = MaskSet
              | testBit params 3 && not (testBit params 4) = MaskClear
              | otherwise = Write

  -- TODO!
  fromStruct (Structs.DCD_Command header (Structs.DCD_Check _ _ _)) = undefined

  fromStruct (Structs.DCD_Command header (Structs.DCD_NOP)) = DCDNOP

  -- stub
  fromStruct (Structs.DCD_Command header (Structs.DCD_Unlock _)) = DCDUnlock

data DCD = DCD [DCDCommand]

data BootData =
  BootData {
    startOffset :: Word32,
    plugin :: Bool}  -- stub!
