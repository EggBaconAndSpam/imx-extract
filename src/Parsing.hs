{- Miscellaneous parsing related definitions. -}

module Parsing (
  manyConstrained,
  getWord8,
  getWord16le,
  getWord16be,
  getWord32le,
  getWord32be,
  SeekableParser,
  seek,
  ByteString,
  ParserT
) where

import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Void
import Data.Word
import Text.Megaparsec (MonadParsec, ParsecT)
import qualified Text.Megaparsec as MP

-- Repeatedly applies the parser until exactly the specified number of tokens
-- have been consumed. Only works with bounded parsers.
manyConstrained :: MonadParsec e s m => m a -> Int -> m [a]
manyConstrained p 0 = return []
manyConstrained p n | n < 0 = fail "constrained parser exceeded bounds"
manyConstrained p n = do
  off <- MP.getOffset
  a <- p
  off' <- MP.getOffset
  as <- manyConstrained p (n - off' + off)
  return (a : as)

-- Convert a list of bytes in little-endian order to the corresponding compound value.
fromBytesLittleEndian :: (Num a, Bits a) => [Word8] -> a
fromBytesLittleEndian = foldr (\byte acc -> shift acc 8 .|. fromIntegral byte) zeroBits

-- Convert a list of bytes in big-endian order to the corresponding compound value.
fromBytesBigEndian :: (Num a, Bits a) => [Word8] -> a
fromBytesBigEndian = fromBytesLittleEndian . reverse

getWord8 :: Ord e => ParsecT e ByteString m Word8
getWord8 = MP.anySingle

getWord16le :: Ord e => ParsecT e ByteString m Word16
getWord16le = fromBytesLittleEndian <$> sequence (replicate 2 getWord8)

getWord16be :: Ord e => ParsecT e ByteString m Word16
getWord16be = fromBytesBigEndian <$> sequence (replicate 2 getWord8)

getWord32le :: Ord e => ParsecT e ByteString m Word32
getWord32le = fromBytesLittleEndian <$> sequence (replicate 4 getWord8)

getWord32be :: Ord e => ParsecT e ByteString m Word32
getWord32be = fromBytesBigEndian <$> sequence (replicate 4 getWord8)

-- megaparsec's parser monad transformer specialised to our use case.
type ParserT = ParsecT Void ByteString

-- A seekable parser monad.
type SeekableParser = ParserT (Reader ByteString)

seek :: Int -> SeekableParser ()
seek offset = do
  input <- lift ask
  let input' = BS.drop offset input

  state <- MP.getParserState
  let statePos = MP.statePosState state
  let sourceName = MP.sourceName $ MP.pstateSourcePos statePos

  -- Update the parser state so that we continue parsing `input'`. We also need
  -- to update the position information. The actual (line,col) position will be
  -- calculated the next time getSourcePos is used.
  let statePos' = statePos { MP.pstateInput = input,
                             MP.pstateOffset = 0,
                             MP.pstateSourcePos = MP.initialPos sourceName,
                             MP.pstateLinePrefix = "" }
  MP.setParserState $ state { MP.stateInput = input',
                        MP.statePosState = statePos' }
