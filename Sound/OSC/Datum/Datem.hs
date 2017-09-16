-- | A class for translating to and from 'Datum'.
module Sound.OSC.Datum.Datem where

import qualified Data.ByteString.Lazy as Lazy {- bytestring -}
import qualified Data.ByteString.Char8 as Char8 {- bytestring -}
import Data.Int {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import Sound.OSC.Datum {- hosc -}
import Sound.OSC.Time {- hosc -}

{- | Class for translating to and from 'Datum'.
     There are instances for the direct 'Datum' field types.

> d_put (1::Int32) == Int32 1
> d_put (1::Int64) == Int64 1
> d_put (1::Float) == Float 1
> d_put (1::Double) == Double 1
> d_put (Char8.pack "str") == ASCII_String (Char8.pack "str")
> d_put (Lazy.pack [37,37]) == Blob (blob_pack [37,37])
> d_put (MIDI 0 0 0 0) == Midi (MIDI 0 0 0 0)

There are also instances for standard Haskell types.

> d_put (1::Int) == Int64 1
> d_put (1::Integer) == Int64 1

-}
class Datem a where
    d_put :: a -> Datum -- ^ Function to wrap value in 'Datum'.
    d_get :: Datum -> Maybe a -- ^ Function to extract value from 'Datum'.

instance Datem Int32 where
    d_put = Int32
    d_get d = case d of {Int32 x -> Just x;_ -> Nothing}

instance Datem Int64 where
    d_put = Int64
    d_get d = case d of {Int64 x -> Just x;_ -> Nothing}

instance Datem Int where
    d_put = Int64 . fromIntegral
    d_get = datum_integral

instance Datem Integer where
    d_put = Int64 . fromIntegral
    d_get = datum_integral

instance Datem Float where
    d_put = Float
    d_get d = case d of {Float x -> Just x;_ -> Nothing}

instance Datem Double where
    d_put = Double
    d_get d = case d of {Double x -> Just x;_ -> Nothing}

instance Datem Char8.ByteString where
    d_put = ASCII_String
    d_get d = case d of {ASCII_String x -> Just x;_ -> Nothing}

instance Datem Lazy.ByteString where
    d_put = Blob
    d_get d = case d of {Blob x -> Just x;_ -> Nothing}

instance Datem MIDI where
    d_put = Midi
    d_get d = case d of {Midi x -> Just x;_ -> Nothing}

-- | Error variant of 'd_get'.
d_get_err :: Datem a => Datum -> a
d_get_err = fromMaybe (error "d_get") . d_get

-- * Type specialised

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
-- > datum_ascii (d_put (Char8.pack "string")) == Just (Char8.pack "string")
datum_ascii :: Datum -> Maybe ASCII
datum_ascii = d_get

-- | 'Char8.unpack' of 'd_get'.
--
-- > datum_string (d_put (Char8.pack "string")) == Just "string"
-- > map datum_string [string "string",Int32 5] == [Just "string",Nothing]
datum_string :: Datum -> Maybe String
datum_string = fmap Char8.unpack . datum_ascii

-- | Type specialised 'd_get'.
datum_blob :: Datum -> Maybe Lazy.ByteString
datum_blob = d_get

-- | 'Maybe' variant of 'd_timestamp'.
datum_timestamp :: Datum -> Maybe Time
datum_timestamp d = case d of {TimeStamp x -> Just x;_ -> Nothing}

-- | Type specialised 'd_get'.
datum_midi :: Datum -> Maybe MIDI
datum_midi = d_get

-- | 'Datum' as sequence of 'Word8' if 'ASCII_String', 'Blob' or 'Midi'.
--
-- > let d = [string "5",Blob (Lazy.pack [53]),midi (0x00,0x90,0x40,0x60)]
-- > in Data.Maybe.mapMaybe datum_sequence d == [[53],[53],[0,144,64,96]]
datum_sequence :: Datum -> Maybe [Word8]
datum_sequence d =
    case d of
      ASCII_String s -> Just (map (fromIntegral . fromEnum) (Char8.unpack s))
      Blob s -> Just (Lazy.unpack s)
      Midi (MIDI p q r s) -> Just [p,q,r,s]
      _ -> Nothing
