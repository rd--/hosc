-- | Optimised decode function for Osc packets.
module Sound.Osc.Coding.Decode.Binary
    (get_packet
    ,decodeMessage
    ,decodeBundle
    ,decodePacket
    ,decodePacket_strict) where

import Control.Applicative {- base -}
import Control.Monad {- base -}
import Data.Int {- base -}
import Data.Word {- base -}

import qualified Data.Binary.Get as Binary {- binary -}
import qualified Data.Binary.IEEE754 as Ieee {- data-binary-ieee754 -}
import qualified Data.ByteString.Lazy as ByteString.Lazy {- bytestring -}
import qualified Data.ByteString.Char8 as ByteString.Char8 {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8 {- bytestring -}

import qualified Sound.Osc.Coding.Byte as Byte {- hosc -}
import Sound.Osc.Coding.Convert {- hosc -}
import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}
import qualified Sound.Osc.Time as Time {- hosc -}

-- | Get a 32 bit integer in big-endian byte order.
getInt32be :: Binary.Get Int32
getInt32be = fmap word32_to_int32 Binary.getWord32be

-- | Get a 64 bit integer in big-endian byte order.
getInt64be :: Binary.Get Int64
getInt64be = fmap word64_to_int64 Binary.getWord64be

-- | Get an aligned Osc string.
get_string :: Binary.Get String
get_string = do
    s <- Binary.getLazyByteStringNul
    Binary.skip (int64_to_int (Byte.align (ByteString.Lazy.length s + 1)))
    return (ByteString.Lazy.Char8.unpack s)

-- | Get an aligned Osc string.
get_ascii :: Binary.Get Ascii
get_ascii = do
    s <- Binary.getLazyByteStringNul
    Binary.skip (int64_to_int (Byte.align (ByteString.Lazy.length s + 1)))
    return (ByteString.Char8.pack (ByteString.Lazy.Char8.unpack s))

-- | Get binary data prefixed by byte count.
get_bytes :: Word32 -> Binary.Get ByteString.Lazy.ByteString
get_bytes n = do
    b <- Binary.getLazyByteString (word32_to_int64 n)
    if n /= int64_to_word32 (ByteString.Lazy.length b)
        then fail "get_bytes: end of stream"
        else Binary.skip (word32_to_int (Byte.align n))
    return b

-- | Get an Osc datum.
get_datum :: DatumType -> Binary.Get Datum
get_datum ty =
    case ty of
      'i' -> fmap Int32 getInt32be
      'h' -> fmap Int64 getInt64be
      'f' -> fmap Float Ieee.getFloat32be
      'd' -> fmap Double Ieee.getFloat64be
      's' -> fmap AsciiString get_ascii
      'b' -> fmap Blob (get_bytes =<< Binary.getWord32be)
      't' -> fmap (TimeStamp . Time.ntpi_to_ntpr) Binary.getWord64be
      'm' -> fmap Midi (liftM4 MidiData Binary.getWord8 Binary.getWord8 Binary.getWord8 Binary.getWord8)
      _ -> fail ("get_datum: illegal type " ++ show ty)

-- | Get an Osc 'Message', fail if type descriptor is invalid.
get_message :: Binary.Get Message
get_message = do
    cmd <- get_string
    dsc <- get_ascii
    case ByteString.Char8.unpack dsc of
        ',':tags -> do
            arg <- mapM get_datum tags
            return (Message cmd arg)
        e -> fail ("get_message: invalid type descriptor string: " ++ e)

-- | Get a sequence of Osc 'Message's, each one headed by its length.
get_message_seq :: Binary.Get [Message]
get_message_seq = do
    b <- Binary.isEmpty
    if b
        then return []
        else do
            p <- flip Binary.isolate get_message . word32_to_int =<< Binary.getWord32be
            ps <- get_message_seq
            return (p:ps)

-- | Get a bundle. Fail if bundle header is not found in packet.
get_bundle :: Binary.Get Bundle
get_bundle = do
    h <- Binary.getByteString (ByteString.Char8.length Byte.bundleHeader_strict)
    when (h /= Byte.bundleHeader_strict) (fail "get_bundle: not a bundle")
    t <- fmap Time.ntpi_to_ntpr Binary.getWord64be
    fmap (Bundle t) get_message_seq

-- | Get an Osc 'Packet'.
get_packet :: Binary.Get Packet
get_packet = fmap Packet_Bundle get_bundle <|> fmap Packet_Message get_message

{-# INLINE decodeMessage #-}
{-# INLINE decodeBundle #-}
{-# INLINE decodePacket #-}
{-# INLINE decodePacket_strict #-}

{- | Decode an Osc 'Message' from a lazy ByteString.

> let b = ByteString.Lazy.pack [47,103,95,102,114,101,101,0,44,105,0,0,0,0,0,0]
> decodeMessage b == Message "/g_free" [Int32 0]
-}
decodeMessage :: ByteString.Lazy.ByteString -> Message
decodeMessage = Binary.runGet get_message

-- | Decode an Osc 'Bundle' from a lazy ByteString.
decodeBundle :: ByteString.Lazy.ByteString -> Bundle
decodeBundle = Binary.runGet get_bundle

{- | Decode an Osc packet from a lazy ByteString.

> let b = ByteString.Lazy.pack [47,103,95,102,114,101,101,0,44,105,0,0,0,0,0,0]
> decodePacket b == Packet_Message (Message "/g_free" [Int32 0])
-}
decodePacket :: ByteString.Lazy.ByteString -> Packet
decodePacket = Binary.runGet get_packet

-- | Decode an Osc packet from a strict Char8 ByteString.
decodePacket_strict :: ByteString.Char8.ByteString -> Packet
decodePacket_strict = Binary.runGet get_packet . ByteString.Lazy.fromChunks . (:[])
