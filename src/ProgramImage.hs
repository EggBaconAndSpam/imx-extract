module ProgramImage where

import Control.Monad.Reader
import Data.Bits
import Data.Word

import Parsing
import qualified Structs
import qualified Text.Megaparsec as Megaparsec

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

instance FromStruct DCD where
  type StructType DCD = Structs.DCD
  fromStruct (Structs.DCD header cmds) = DCD (map fromStruct cmds)

data BootData =
  BootData {
    startOffset :: Word32,
    plugin :: Bool}  -- stub!

instance FromStruct BootData where
  type StructType BootData = Structs.Boot_Data
  fromStruct (Structs.Boot_Data off _ plug) =
    BootData off (if | plug == 0 -> False
                     | otherwise -> True)

data ProgramImage = ProgramImage BootData DCD

extractProgramImage :: ByteString -> Maybe ProgramImage
extractProgramImage = runReader $ do
  file <- ask
  eResult <- Megaparsec.runParserT parseProgramImage "" file
  case eResult of
    Right pi -> return (Just pi)
    _ -> return Nothing
 where
  parseProgramImage = do
    header :: Structs.IVT <- Structs.struct
    seek (fromIntegral $ Structs.ivt_boot_data header)
    bootData :: BootData <- pure fromStruct <*> Structs.struct
    seek (fromIntegral $ Structs.ivt_dcd header)
    dcd :: DCD <- pure fromStruct <*> Structs.struct
    return (ProgramImage bootData dcd)
