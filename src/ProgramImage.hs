module ProgramImage where

import Control.Monad.Reader
import Data.Bits
import Data.Word
import qualified Text.Megaparsec as Megaparsec
import Text.Printf

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

instance Show DCDCommand where
  show (DCDWrite width mode assocs) = concatMap go assocs
   where
    mode' = case mode of
        Write -> "DATA"
        MaskSet -> "SET_BIT"
        MaskClear -> "CLR_BIT"
    go (addr, dat) = printf "%s %d 0x%08X 0x%08X\n" mode' width addr dat

  show (DCDCheck width mode addr mask Nothing) =
    printf "%s %d 0x%08X 0x%08X\n" mode' width addr mask
   where
    mode' = case mode of
        CheckSet -> "CHECK_BITS_SET"
        CheckClear -> "CHECK_BITS_CLR"

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

instance Show DCD where
  show (DCD cmds) = concatMap show cmds

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

instance Show ProgramImage where
  show (ProgramImage _ dcd) = "BOOT_FROM sd\n\n" ++ show dcd

extractProgramImage :: Int -> Int -> ByteString -> Maybe ProgramImage
extractProgramImage headerOffset textBase = runReader $ do
  file <- ask
  eResult <- Megaparsec.runParserT parseProgramImage "" file
  case eResult of
    Right pi -> return (Just pi)
    Left errs -> error (show errs)
 where
  parseProgramImage = do
    seek headerOffset
    header :: Structs.IVT <- Structs.struct
    seek $ fromIntegral (Structs.ivt_boot_data header) - textBase
    bootData :: BootData <- pure fromStruct <*> Structs.struct
    seek $ fromIntegral (Structs.ivt_dcd header) - textBase
    dcd :: DCD <- pure fromStruct <*> Structs.struct
    return (ProgramImage bootData dcd)
