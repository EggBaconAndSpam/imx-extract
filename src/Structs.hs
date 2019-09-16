{- This module contains the datatypes we are going to be parsing in their
physical representation, mirroring the presentation in the IMX manual. These are
translated, in ProgramImage.hs, to a more semantic representation. -}
module Structs where

import Data.ByteString (ByteString)
import Data.Word
import qualified Data.ByteString as ByteString
import qualified Text.Megaparsec as Megaparsec

import Parsing

-- A Haskell encoding of a C-style 'struct', with an associated parser.
class Struct a where
  struct :: ParserT m a

-- The general 'header' structure; appears in a couple of places.
data Header = Header { header_tag :: Word8,
                       header_length :: Word16,
                       header_parameter :: Word8 }

instance Struct Header where
  struct = Header <$> getWord8 <*> getWord16be <*> getWord8

-- The 'Image Vector Table'; a list of addresses to the actual data.
data IVT = IVT { ivt_header :: Header,
                 ivt_entry,
                 ivt_reserved1,
                 ivt_dcd,
                 ivt_boot_data,
                 ivt_self,
                 ivt_csf,
                 ivt_reserved2 :: Word32 }

instance Struct IVT where
  struct =
    IVT <$> struct <*> getWord32le <*> getWord32le
        <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le
        <*> getWord32le

-- Boot data, i.e. location and size of the first-stage boot loader.
data Boot_Data = Boot_Data { boot_data_start,
                             boot_data_length,
                             boot_data_plugin :: Word32 }

instance Struct Boot_Data where
  struct = Boot_Data <$> getWord32le <*> getWord32le <*> getWord32le

-- A 'Device Configuration Data' command. Write commands for example allow us to
-- initialize the RAM controller before the bootloader runs.
data DCD_Command = DCD_Command Header DCD_Command_Payload
data DCD_Command_Payload =
  DCD_Write [(Word32, Word32)] |
  DCD_Check { dcd_check_address :: Word32,
              dcd_check_mask :: Word32,
              dcd_count :: Maybe Word32 } |
  DCD_NOP |
  DCD_Unlock [Word32]

parseDCDPayload :: Header -> ParserT m DCD_Command_Payload
parseDCDPayload (Header 0xCC header_length _) = do
  let cnt = fromIntegral $ (header_length - 4) `div` 8
  let getAddressValue = (,) <$> getWord32be <*> getWord32be
  DCD_Write <$> sequence (replicate cnt getAddressValue)
parseDCDPayload (Header 0xCF header_length _) =
  DCD_Check <$> getWord32be <*> getWord32be
            <*> if | header_length == 16 -> Just <$> getWord32be
                   | otherwise -> return Nothing
parseDCDPayload (Header 0xC0 _ _) = return DCD_NOP
parseDCDPayload (Header 0xB2 header_length _) = do
  let cnt = fromIntegral $ (header_length - 4) `div` 4
  DCD_Unlock <$> sequence (replicate cnt getWord32be)

instance Struct DCD_Command where
  struct = do
    header <- struct
    payload <- parseDCDPayload header
    return (DCD_Command header payload)

-- The 'Device Configuration Data' is a list of commands that are executed
-- before the bootloader runs.
data DCD = DCD Header [DCD_Command]

instance Struct DCD where
  struct = do
    header <- struct
    commands <- manyConstrained struct (fromIntegral (header_length header) - 4)
    return (DCD header commands)
