module Parsing (
  manyConstrained,
  getWord8,
  getWord16le,
  getWord16be,
  getWord32le,
  getWord32be,
  FileParser,
  seek,
  ByteString,
  ParsecT,
  ParseT
) where

import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Void
import Data.Word
import Text.Megaparsec (MonadParsec, ParsecT)
import qualified Text.Megaparsec as Megaparsec

-- Repeatedly applies the parser until exactly the specified number of tokens
-- have been consumed. Only works with bounded parsers.
manyConstrained :: MonadParsec e s m => m a -> Int -> m [a]
manyConstrained p 0 = return []
manyConstrained p n | n < 0 = fail "constrained parser exceeded bounds"
manyConstrained p n = do
  off <- Megaparsec.getOffset
  a <- p
  off' <- Megaparsec.getOffset
  as <- manyConstrained p (n - off' + off)
  return (a : as)

fromBytesLittleEndian :: (Num a, Bits a) => [Word8] -> a
fromBytesLittleEndian = foldr (\byte acc -> shift acc 8 .|. fromIntegral byte) zeroBits

fromBytesBigEndian :: (Num a, Bits a) => [Word8] -> a
fromBytesBigEndian = fromBytesLittleEndian . reverse

getWord8 :: Ord e => ParsecT e ByteString m Word8
getWord8 = Megaparsec.anySingle

getWord16le :: Ord e => ParsecT e ByteString m Word16
getWord16le = fromBytesLittleEndian <$> sequence (replicate 2 getWord8)

getWord16be :: Ord e => ParsecT e ByteString m Word16
getWord16be = fromBytesBigEndian <$> sequence (replicate 2 getWord8)

getWord32le :: Ord e => ParsecT e ByteString m Word32
getWord32le = fromBytesLittleEndian <$> sequence (replicate 4 getWord8)

getWord32be :: Ord e => ParsecT e ByteString m Word32
getWord32be = fromBytesBigEndian <$> sequence (replicate 4 getWord8)

type ParseT = ParsecT Void ByteString

type FileParser = ParsecT Void ByteString (Reader ByteString)

seek :: Int -> FileParser ()
seek off = do
  file <- lift ask
  Megaparsec.setInput (ByteString.drop off file)
  Megaparsec.setOffset off
