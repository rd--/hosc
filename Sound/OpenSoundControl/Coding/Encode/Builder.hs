-- | Encode functions for OSC packets.
module Sound.OpenSoundControl.Coding.Encode.Builder ( buildOSC
                                                    , encodeOSC
                                                    , encodeOSC' ) where

import qualified Data.Binary.IEEE754 as I
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B
import Data.Monoid (mappend, mconcat)
import Data.Word (Word8)
import Sound.OpenSoundControl.Coding.Byte (align, bundleHeader)
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Type (Datum(..), OSC(..), tag)

-- Command argument types are given by a descriptor.
descriptor :: [Datum] -> String
descriptor l = ',' : map tag l

-- Generate a list of zero bytes for padding.
padding :: Integral i => i -> [Word8]
padding n = replicate (fromIntegral n) 0

-- Encode a string with zero padding.
build_string :: String -> B.Builder
build_string s = B.fromString s `mappend` B.fromWord8s (0:padding (align (length s + 1)))

-- Encode a byte string with prepended length and zero padding.
build_bytes :: L.ByteString -> B.Builder
build_bytes s = B.fromInt32be (fromIntegral (L.length s))
                `mappend` B.fromLazyByteString s
                `mappend` B.fromWord8s (padding (align (L.length s)))

-- Encode an OSC datum.
build_datum :: Datum -> B.Builder
build_datum (Int i)              = B.fromInt32be (fromIntegral i)
build_datum (Float f)            = B.fromWord32be (I.floatToWord (realToFrac f))
build_datum (Double d)           = B.fromWord64be (I.doubleToWord d)
build_datum (TimeStamp t)        = B.fromWord64be (fromIntegral (as_ntpi t))
build_datum (String s)           = build_string s
build_datum (Midi (b0,b1,b2,b3)) = B.fromWord8s [b0,b1,b2,b3]
build_datum (Blob b)             = build_bytes b

-- Encode an OSC message.
build_message :: String -> [Datum] -> B.Builder
build_message c l =
    mconcat [ build_string c
            , build_string (descriptor l)
            , mconcat $ map build_datum l ]

-- Encode an OSC bundle.
build_bundle_ntpi :: NTPi -> [OSC] -> B.Builder
build_bundle_ntpi t l =
    mconcat [ B.fromLazyByteString bundleHeader
            , B.fromWord64be t
            , mconcat $ map (build_bytes . B.toLazyByteString . buildOSC) l ]

-- | Builder monoid for an OSC packet.
buildOSC :: OSC -> B.Builder
buildOSC (Message c l)       = build_message c l
buildOSC (Bundle (NTPi t) l) = build_bundle_ntpi t l
buildOSC (Bundle (NTPr t) l) = build_bundle_ntpi (ntpr_ntpi t) l
buildOSC (Bundle (UTCr t) l) = build_bundle_ntpi (utcr_ntpi t) l

-- | Encode an OSC packet to a lazy ByteString.
encodeOSC :: OSC -> L.ByteString
{-# INLINE encodeOSC #-}
encodeOSC = B.toLazyByteString . buildOSC

-- | Encode an OSC packet to a strict ByteString.
encodeOSC' :: OSC -> S.ByteString
{-# INLINE encodeOSC' #-}
encodeOSC' = B.toByteString . buildOSC
