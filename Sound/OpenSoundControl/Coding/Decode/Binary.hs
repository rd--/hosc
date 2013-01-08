-- | Optimised decode function for OSC packets.
module Sound.OpenSoundControl.Coding.Decode.Binary
    (getPacket
    ,decodePacket
    ,decodePacket_strict) where

import Control.Applicative
import Data.Binary.Get
import qualified Data.Binary.IEEE754 as I
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Int (Int32)
import Data.Word (Word32)
import Sound.OpenSoundControl.Coding.Byte
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Type

-- | Isolate an action to operating within a fixed block of bytes. The action
--   is required to consume all the bytes that it is isolated to.
isolate :: Word32 -> Get a -> Get a
isolate n m = do
    s <- get_bytes n
    let (a, s', _) = runGetState m s 0
    if L.null s'
        then return a
        else fail "isolate: not all bytes consumed"

-- | Get a 32 bit integer in big-endian byte order.
getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

-- | Get an aligned OSC string.
get_string :: Get String
get_string = do
    s <- getLazyByteStringNul
    skip (fromIntegral (align (L.length s + 1)))
    return $ C.unpack s

-- | Get binary data prefixed by byte count.
get_bytes :: Word32 -> Get L.ByteString
get_bytes n = do
    b <- getLazyByteString (fromIntegral n)
    if n /= fromIntegral (L.length b)
        then fail "get_bytes: end of stream"
        else skip (fromIntegral (align n))
    return b

-- | Get an OSC datum.
get_datum :: Datum_Type -> Get Datum
get_datum ty =
    case ty of
      'i' -> Int    <$> fromIntegral <$> getInt32be
      'f' -> Float  <$> realToFrac <$> I.getFloat32be
      'd' -> Double <$> I.getFloat64be
      's' -> String <$> get_string
      'b' -> Blob   <$> (get_bytes =<< getWord32be)
      't' -> TimeStamp <$> ntpi_to_ntpr <$> getWord64be
      'm' -> do b0 <- getWord8
                b1 <- getWord8
                b2 <- getWord8
                b3 <- getWord8
                return $ Midi (b0,b1,b2,b3)
      _ -> fail ("get_datum: illegal type " ++ show ty)

-- | Get an OSC 'Message'.
get_message :: Get Message
get_message = do
    cmd <- get_string
    dsc <- get_string
    case dsc of
        (',':tags) -> do
            arg <- mapM get_datum tags
            return $ Message cmd arg
        _ -> fail "get_message: invalid type descriptor string"

-- | Get a sequence of OSC 'Message's, each one headed by its length.
get_message_seq :: Get [Message]
get_message_seq = do
    b <- isEmpty
    if b
        then return []
        else do
            p <- flip isolate get_message =<< getWord32be
            ps <- get_message_seq
            return (p:ps)

get_bundle :: Get Bundle
get_bundle = do
    skip (fromIntegral (L.length bundleHeader))
    t <- ntpi_to_ntpr <$> getWord64be
    ps <- get_message_seq
    return $ Bundle t ps

-- | Get an OSC 'Packet'.
getPacket :: Get Packet
getPacket = do
    h <- uncheckedLookAhead (L.length bundleHeader)
    if h == bundleHeader
        then fmap Packet_Bundle get_bundle
        else fmap Packet_Message get_message


-- | Decode an OSC packet from a lazy ByteString.
--
-- > let b = L.pack [47,103,95,102,114,101,101,0,44,105,0,0,0,0,0,0]
-- > in decodeOSC b == Message "/g_free" [Int 0]
decodePacket :: L.ByteString -> Packet
{-# INLINE decodePacket #-}
decodePacket = runGet getPacket

-- | Decode an OSC packet from a strict ByteString.
decodePacket_strict :: S.ByteString -> Packet
{-# INLINE decodePacket_strict #-}
decodePacket_strict = runGet getPacket . L.fromChunks . (:[])
