-- | Alegbraic data types for OSC packets and encode and decode
--   functions.
module Sound.OpenSoundControl.OSC.Builder ( buildOSC ) where

import qualified Data.ByteString.Lazy as B
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B
import Data.Monoid (mappend, mconcat)
import Data.Word (Word8)
import Sound.OpenSoundControl.OSC.Type (Datum(..), OSC(..), tag)
import Sound.OpenSoundControl.Byte (align, bundleHeader)
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Cast

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
build_bytes :: B.ByteString -> B.Builder
build_bytes s = B.fromInt32be (fromIntegral (B.length s))
                `mappend` B.fromLazyByteString s
                `mappend` B.fromWord8s (padding (align (B.length s)))

-- Encode an OSC datum.
build_datum :: Datum -> B.Builder
build_datum (Int i) = B.fromInt32be (fromIntegral i)
build_datum (Float f) = B.fromInt32be (f32_i32 (realToFrac f))
build_datum (Double d) = B.fromInt64be (f64_i64 d)
build_datum (TimeStamp t) = B.fromWord64be (fromIntegral (as_ntpi t))
build_datum (String s) = build_string s
build_datum (Midi (b0,b1,b2,b3)) = B.fromWord8s [b0,b1,b2,b3]
build_datum (Blob b) = build_bytes b

-- Encode an OSC message.
build_message :: String -> [Datum] -> B.Builder
build_message c l =
    mconcat $
        [
          build_string c
        , build_string (descriptor l)
        ] ++ map build_datum l

-- Encode an OSC bundle.
build_bundle_ntpi :: NTPi -> [OSC] -> B.Builder
build_bundle_ntpi t l =
    mconcat $ [ B.fromLazyByteString bundleHeader
              , B.fromWord64be t ]
              ++ map (build_bytes . B.toLazyByteString . buildOSC) l

-- | Builder monoid for an OSC packet.
buildOSC :: OSC -> B.Builder
buildOSC (Message c l)       = build_message c l
buildOSC (Bundle (NTPi t) l) = build_bundle_ntpi t l
buildOSC (Bundle (NTPr t) l) = build_bundle_ntpi (ntpr_ntpi t) l
buildOSC (Bundle (UTCr t) l) = build_bundle_ntpi (utcr_ntpi t) l
