-- | Binary get and put functions for 'OSC'.
module Sound.OpenSoundControl.OSC.Binary ( getOSC
                                         , decodeOSC
                                         , decodeOSC' ) where

import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Int (Int32)
import Data.Word (Word32)
import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.OSC.Type
import Sound.OpenSoundControl.Time

-- | Isolate an action to operating within a fixed block of bytes. The action
--   is required to consume all the bytes that it is isolated to.
isolate :: Word32 -> Get a -> Get a
isolate n m = do
    s <- get_bytes n
    let (a, s', _) = runGetState m s 0
    if B.null s'
        then return a
        else fail ("isolate: not all bytes consumed")

-- | Get a 32 biut integer in big-endian byte order.
getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

-- | Get an aligned OSC string.
get_string :: Get String
get_string = do
    s <- getLazyByteStringNul
    skip (fromIntegral (align (B.length s + 1)))
    return $ BC.unpack s

-- | Get binary data prefixed by byte count.
get_bytes :: Word32 -> Get B.ByteString
get_bytes n = do
    b <- getLazyByteString (fromIntegral n)
    if n /= fromIntegral (B.length b)
        then fail ("get_bytes: end of stream")
        else skip (fromIntegral (align n))
    return b

-- | Get an OSC datum.
get_datum :: Char -> Get Datum
get_datum 'i' = Int    <$> fromIntegral <$> getInt32be
get_datum 'f' = Float  <$> realToFrac <$> getFloat32be
get_datum 'd' = Double <$> getFloat64be
get_datum 's' = String <$> get_string
get_datum 'b' = Blob   <$> (get_bytes =<< getWord32be)
get_datum 't' = TimeStamp <$> NTPi <$> getWord64be
get_datum 'm' = do
    b0 <- getWord8
    b1 <- getWord8
    b2 <- getWord8
    b3 <- getWord8
    return $ Midi (b0,b1,b2,b3)
get_datum t = fail ("get_datum: illegal type " ++ show t)

-- | Get an OSC message.
get_message :: Get OSC
get_message = do
    cmd <- get_string
    dsc <- get_string
    case dsc of
        (',':tags) -> do
            arg <- mapM get_datum tags
            return $ Message cmd arg
        _ -> fail ("get_message: invalid type descriptor string")

-- | Get an OSC packet.
get_packet :: Get OSC
get_packet = do
    h <- uncheckedLookAhead (B.length bundleHeader)
    if h == bundleHeader
        then get_bundle
        else get_message

-- | Get a sequence of OSC messages, each one headed by its length.
get_packet_seq :: Get [OSC]
get_packet_seq = do
    b <- isEmpty
    if b
        then return []
        else do
            p <- flip isolate get_packet =<< getWord32be
            ps <- get_packet_seq
            return (p:ps)

get_bundle :: Get OSC
get_bundle = do
    skip (fromIntegral (B.length bundleHeader))
    t <- NTPi <$> getWord64be
    ps <- get_packet_seq
    return $ Bundle t ps

-- | Get an OSC packet.
getOSC :: Get OSC
getOSC = get_packet

decodeOSC :: B.ByteString -> OSC
{-# INLINE decodeOSC #-}
decodeOSC = runGet getOSC

decodeOSC' :: BS.ByteString -> OSC
{-# INLINE decodeOSC' #-}
decodeOSC' = runGet getOSC . B.fromChunks . (:[])
