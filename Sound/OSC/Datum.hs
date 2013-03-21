-- | 'Datum' related functions.
module Sound.OSC.Datum where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import Data.Int {- base -}
import Data.Word {- base -}

import Sound.OSC.Type

-- | Type specialised 'd_get'.
--
-- > map datum_int32 [Int32 1,Float 1] == [Just 1,Nothing]
datum_int32 :: Datum -> Maybe Int32
datum_int32 = d_get

-- | Type specialised 'd_get'.
datum_int64 :: Datum -> Maybe Int64
datum_int64 = d_get

-- | Type specialised 'd_get'.
datum_float :: Datum -> Maybe Float
datum_float = d_get

-- | Type specialised 'd_get'.
datum_double :: Datum -> Maybe Double
datum_double = d_get

-- | Type specialised 'd_get'.
--
-- > datum_ascii (d_put (C.pack "string")) == Just (C.pack "string")
datum_ascii :: Datum -> Maybe ASCII
datum_ascii = d_get

-- | 'C.unpack' of 'd_get'.
--
-- > datum_string (d_put (C.pack "string")) == Just "string"
datum_string :: Datum -> Maybe String
datum_string = fmap C.unpack . datum_ascii

-- | Type specialised 'd_get'.
datum_blob :: Datum -> Maybe B.ByteString
datum_blob = d_get

-- | 'Maybe' variant of 'd_timestamp'.
datum_timestamp :: Datum -> Maybe Time
datum_timestamp d = case d of {TimeStamp x -> Just x;_ -> Nothing}

-- | Type specialised 'd_get'.
datum_midi :: Datum -> Maybe MIDI
datum_midi = d_get

-- | 'Datum' as sequence of 'Word8' if 'ASCII_String', 'Blob' or 'Midi'.
--
-- > let d = [string "5",Blob (B.pack [53]),midi (0x00,0x90,0x40,0x60)]
-- > in Data.Maybe.mapMaybe datum_sequence d == [[53],[53],[0,144,64,96]]
datum_sequence :: Datum -> Maybe [Word8]
datum_sequence d =
    case d of
      ASCII_String s -> Just (map (fromIntegral . fromEnum) (C.unpack s))
      Blob s -> Just (B.unpack s)
      Midi (MIDI p q r s) -> Just [p,q,r,s]
      _ -> Nothing
