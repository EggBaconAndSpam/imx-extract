module ProgramImage where

import Control.Monad.Reader
import Data.Bits
import Data.Word
import qualified Text.Megaparsec as Megaparsec
import Text.Printf

import Parsing
import qualified Structs

-- A more semantic representation of a `Struct` (see Struct.hs), with a
-- conversion function.
class FromStruct a where
  type StructType a
  fromStruct :: StructType a -> a

data DCDWriteMode = Write | MaskSet | MaskClear

data DCDCheckMode = CheckClear | CheckSet | CheckAnySet | CheckAnyClear

data DCDCommand =
  DCDWrite { byteWidth :: Int,
             writeMode :: DCDWriteMode,
             -- (address, value) pairs
             associations :: [(Word32, Word32)] } |
  DCDCheck { byteWidth :: Int,
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

  -- TBD (not by me though); not used by the Tolino images.
  fromStruct (Structs.DCD_Command header (Structs.DCD_Check _ _ _)) = undefined

  fromStruct (Structs.DCD_Command header (Structs.DCD_NOP)) = DCDNOP

  -- Not even supported by imximage.
  fromStruct (Structs.DCD_Command header (Structs.DCD_Unlock _)) = DCDUnlock

data DCD = DCD [DCDCommand]

instance Show DCD where
  show (DCD cmds) = concatMap show cmds

instance FromStruct DCD where
  type StructType DCD = Structs.DCD
  fromStruct (Structs.DCD header cmds) = DCD (map fromStruct cmds)

-- Given an image dump, extract the DCD. Use the show instance to convert to the
-- format used by imximage.
extractDCD :: Int -> Int -> ByteString -> Maybe DCD
extractDCD headerOffset textBase img = flip runReader img $ do
  result <- Megaparsec.runParserT parseDCDFromImage "" img
  case result of
    Right pi -> return (Just pi)
    Left _ -> return Nothing
 where
  parseDCDFromImage = do
    seek headerOffset
    header :: Structs.IVT <- Structs.struct
    seek $ fromIntegral (Structs.ivt_dcd header) - textBase
    dcd :: DCD <- pure fromStruct <*> Structs.struct
    return dcd

-- headerBase = 0x400 and textbase = 0x8780000 for SD/MMC images on i.mx6
extractDCDFromSDImage :: ByteString -> Maybe DCD
extractDCDFromSDImage = extractDCD 0x400 0x87800000
