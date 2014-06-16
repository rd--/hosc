-- | Optimised decode function for OSC packets.
module Sound.OSC.Coding.Decode.Binary
    (getPacket
    ,decodePacket
    ,decodePacket_strict) where

import Control.Applicative {- base -}
import Control.Monad (when) {- base -}
import qualified Data.Binary.Get as G {- binary -}
import qualified Data.Binary.IEEE754 as I {- data-binary-ieee754 -}
import qualified Data.ByteString.Char8 as S.C {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as C {- bytestring -}
import Data.Int {- base -}
import Data.Word {- base -}

import Sound.OSC.Coding.Byte
import Sound.OSC.Time
import Sound.OSC.Type

-- | Get a 32 bit integer in big-endian byte order.
getInt32be :: G.Get Int32
getInt32be = fromIntegral <$> G.getWord32be

-- | Get a 64 bit integer in big-endian byte order.
getInt64be :: G.Get Int64
getInt64be = fromIntegral <$> G.getWord64be

-- | Get an aligned OSC string.
get_string :: G.Get String
get_string = do
    s <- G.getLazyByteStringNul
    G.skip (fromIntegral (align (B.length s + 1)))
    return $ C.unpack s

-- | Get an aligned OSC string.
get_ascii :: G.Get ASCII
get_ascii = do
    s <- G.getLazyByteStringNul
    G.skip (fromIntegral (align (B.length s + 1)))
    return (S.C.pack (C.unpack s))

-- | Get binary data prefixed by byte count.
get_bytes :: Word32 -> G.Get B.ByteString
get_bytes n = do
    b <- G.getLazyByteString (fromIntegral n)
    if n /= fromIntegral (B.length b)
        then fail "get_bytes: end of stream"
        else G.skip (fromIntegral (align n))
    return b

-- | Get an OSC datum.
get_datum :: Datum_Type -> G.Get Datum
get_datum ty =
    case ty of
      'i' -> Int32  <$> fromIntegral <$> getInt32be
      'h' -> Int64  <$> fromIntegral <$> getInt64be
      'f' -> Float  <$> realToFrac <$> I.getFloat32be
      'd' -> Double <$> I.getFloat64be
      's' -> ASCII_String <$> get_ascii
      'b' -> Blob   <$> (get_bytes =<< G.getWord32be)
      't' -> TimeStamp <$> ntpi_to_ntpr <$> G.getWord64be
      'm' -> do b0 <- G.getWord8
                b1 <- G.getWord8
                b2 <- G.getWord8
                b3 <- G.getWord8
                return $ Midi (MIDI b0 b1 b2 b3)
      _ -> fail ("get_datum: illegal type " ++ show ty)

-- | Get an OSC 'Message'.
get_message :: G.Get Message
get_message = do
    cmd <- get_string
    dsc <- get_ascii
    case S.C.unpack dsc of
        ',':tags -> do
            arg <- mapM get_datum tags
            return $ Message cmd arg
        _ -> fail "get_message: invalid type descriptor string"

-- | Get a sequence of OSC 'Message's, each one headed by its length.
get_message_seq :: G.Get [Message]
get_message_seq = do
    b <- G.isEmpty
    if b
        then return []
        else do
            p <- flip G.isolate get_message . fromIntegral =<< G.getWord32be
            ps <- get_message_seq
            return (p:ps)

-- | Get a bundle. Fail if bundle header is not found in packet.
get_bundle :: G.Get Bundle
get_bundle = do
    h <- G.getByteString (S.C.length bundleHeader_strict)
    when (h /= bundleHeader_strict) (fail "get_bundle: not a bundle")
    t <- ntpi_to_ntpr <$> G.getWord64be
    ps <- get_message_seq
    return $ Bundle t ps

-- | Get an OSC 'Packet'.
getPacket :: G.Get Packet
getPacket = (Packet_Bundle <$> get_bundle) <|> (Packet_Message <$> get_message)

-- | Decode an OSC packet from a lazy ByteString.
--
-- > let b = B.pack [47,103,95,102,114,101,101,0,44,105,0,0,0,0,0,0]
-- > in decodeOSC b == Message "/g_free" [Int 0]
decodePacket :: B.ByteString -> Packet
{-# INLINE decodePacket #-}
decodePacket = G.runGet getPacket

-- | Decode an OSC packet from a strict ByteString.
decodePacket_strict :: S.C.ByteString -> Packet
{-# INLINE decodePacket_strict #-}
decodePacket_strict = G.runGet getPacket . B.fromChunks . (:[])
