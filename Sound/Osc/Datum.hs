-- | Osc data types.
module Sound.Osc.Datum where

import Data.Int {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Data.ByteString.Char8 as ByteString.Char8 {- bytestring -}
import qualified Data.ByteString.Lazy as ByteString.Lazy {- bytestring -}

-- * Datum

-- | Type enumerating Datum categories.
type DatumType = Char

-- | Type for Ascii strings (strict Char8 ByteString)
type Ascii = ByteString.Char8.ByteString

-- | Type-specialised pack.
ascii :: String -> Ascii
ascii = ByteString.Char8.pack

-- | Type-specialised unpack.
ascii_to_string :: Ascii -> String
ascii_to_string = ByteString.Char8.unpack

-- | Type for 'Word8' arrays, these are stored with an 'Int32' length prefix.
type Blob = ByteString.Lazy.ByteString

-- | Type-specialised pack.
blob_pack :: [Word8] -> Blob
blob_pack = ByteString.Lazy.pack

-- | Type-specialised pack.
blob_pack_int :: [Int] -> Blob
blob_pack_int = ByteString.Lazy.pack . map fromIntegral

-- | Type-specialised unpack.
blob_unpack :: Blob -> [Word8]
blob_unpack = ByteString.Lazy.unpack

-- | Type-specialised unpack.
blob_unpack_int :: Blob -> [Int]
blob_unpack_int = map fromIntegral . blob_unpack

-- | Four-byte midi message: port-id, status-byte, data, data.
data MidiData = MidiData !Word8 !Word8 !Word8 !Word8
  deriving (Ord, Eq, Show, Read)

midi_pack :: [Word8] -> MidiData
midi_pack w =
  case w of
    [m1, m2, m3, m4] -> MidiData m1 m2 m3 m4
    _ -> error "midi_pack?"

-- | Type-specialised pack.
midi_pack_int :: [Int] -> MidiData
midi_pack_int = midi_pack . map fromIntegral

-- | Type-specialised unpack.
midi_unpack_int :: MidiData -> [Int]
midi_unpack_int (MidiData m1 m2 m3 m4) = map fromIntegral [m1, m2, m3, m4]

{- | A real-valued time stamp.
For Osc proper this is an Ntp64 time in real-valued (fractional) form.
For SuperCollider Nrt programs this is elapsed time since the start of the score.
This is the primary form of timestamp used by hosc.
-}
type Time = Double

-- | The basic elements of Osc messages.
data Datum
  = Int32 {d_int32 :: !Int32}
  | Int64 {d_int64 :: !Int64}
  | Float {d_float :: !Float}
  | Double {d_double :: !Double}
  | AsciiString {d_ascii_string :: !Ascii}
  | Blob {d_blob :: !Blob}
  | TimeStamp {d_timestamp :: !Time} -- ie. real valued Ntp
  | Midi {d_midi :: !MidiData}
  deriving (Ord, Eq, Read, Show)

-- * Datum types

-- | List of required data types (tag, name).
osc_types_required :: [(DatumType, String)]
osc_types_required =
  [ ('i', "Int32")
  , ('f', "Float")
  , ('s', "String") -- Ascii
  , ('b', "Blob")
  ]

-- | List of optional data types (tag,name).
osc_types_optional :: [(DatumType, String)]
osc_types_optional =
  [ ('h', "Int64")
  , ('t', "TimeStamp")
  , ('d', "Double")
  , -- ,('S',"Symbol")
    -- ,('c',"Character")
    -- ,('r',"RGBA")
    ('m', "Midi")
    -- ,('T',"True")
    -- ,('F',"False")
    -- ,('N',"Nil")
    -- ,('I',"Infinitum")
    -- ,('[',"Array_Begin")
    -- ,(']',"Array_End")
  ]

-- | List of all data types (tag,name).
osc_types :: [(DatumType, String)]
osc_types = osc_types_required ++ osc_types_optional

-- | Lookup name of type.
osc_type_name :: DatumType -> Maybe String
osc_type_name c = lookup c osc_types

-- | Erroring variant.
osc_type_name_err :: DatumType -> String
osc_type_name_err = fromMaybe (error "osc_type_name") . osc_type_name

-- | Single character identifier of an Osc datum.
datum_tag :: Datum -> DatumType
datum_tag d =
  case d of
    Int32 _ -> 'i'
    Int64 _ -> 'h'
    Float _ -> 'f'
    Double _ -> 'd'
    AsciiString _ -> 's'
    Blob _ -> 'b'
    TimeStamp _ -> 't'
    Midi _ -> 'm'

-- | Type and name of 'Datum'.
datum_type_name :: Datum -> (DatumType, String)
datum_type_name d = let c = datum_tag d in (c, osc_type_name_err c)

-- * Generalised element access

{- | 'Datum' as 'Integral' if Int32 or Int64.

>>> let d = [Int32 5,Int64 5,Float 5.5,Double 5.5]
>>> map datum_integral d == [Just (5::Int),Just 5,Nothing,Nothing]
True
-}
datum_integral :: Integral i => Datum -> Maybe i
datum_integral d =
  case d of
    Int32 x -> Just (fromIntegral x)
    Int64 x -> Just (fromIntegral x)
    _ -> Nothing

{- | 'Datum' as 'Floating' if Int32, Int64, Float, Double or TimeStamp.

>>> let d = [Int32 5,Int64 5,Float 5,Double 5,TimeStamp 5]
>>> mapMaybe datum_floating d == replicate 5 (5::Double)
True
-}
datum_floating :: Floating n => Datum -> Maybe n
datum_floating d =
  case d of
    Int32 n -> Just (fromIntegral n)
    Int64 n -> Just (fromIntegral n)
    Float n -> Just (realToFrac n)
    Double n -> Just (realToFrac n)
    TimeStamp n -> Just (realToFrac n)
    _ -> Nothing

-- * Constructors

{- | Type generalised 'Int32'.

>>> int32 (1::Int32) == int32 (1::Integer)
True

>>> d_int32 (int32 (maxBound::Int32)) == maxBound
True

>>> int32 (((2::Int) ^ (64::Int))::Int) == Int32 0
True
-}
int32 :: Integral n => n -> Datum
int32 = Int32 . fromIntegral

{- | Type generalised Int64.

>>> int64 (1::Int32) == int64 (1::Integer)
True

>>> d_int64 (int64 (maxBound::Int64)) == maxBound
True
-}
int64 :: Integral n => n -> Datum
int64 = Int64 . fromIntegral

{- | Type generalised Float.

>>> float (1::Int) == float (1::Double)
True

>>> floatRange (undefined::Float)
(-125,128)

>>> isInfinite (d_float (float (encodeFloat 1 256 :: Double)))
True
-}
float :: Real n => n -> Datum
float = Float . realToFrac

{- | Type generalised Double.

>>> double (1::Int) == double (1::Double)
True

>>> double (encodeFloat 1 256 :: Double) == Double 1.157920892373162e77
True
-}
double :: Real n => n -> Datum
double = Double . realToFrac

{- | 'AsciiString' of pack.

>>> string "string" == AsciiString (ByteString.Char8.pack "string")
True
-}
string :: String -> Datum
string = AsciiString . ascii

{- | Four-tuple variant of 'Midi' '.' 'MidiData'.

>>> midi (0,0,0,0) == Midi (MidiData 0 0 0 0)
True
-}
midi :: (Word8, Word8, Word8, Word8) -> Datum
midi (p, q, r, s) = Midi (MidiData p q r s)

-- | 'Blob' of 'blob_pack'.
blob :: [Word8] -> Datum
blob = Blob . blob_pack

-- * Descriptor

{- | Message argument types are given by a signature.

>>> signatureFor [Int32 1,Float 1,string "1"]
",ifs"
-}
signatureFor :: [Datum] -> String
signatureFor = (',' :) . map datum_tag

{- | The descriptor is an Ascii encoded signature.

>>> descriptor [Int32 1,Float 1,string "1"] == ascii ",ifs"
True
-}
descriptor :: [Datum] -> Ascii
descriptor = ascii . signatureFor

-- | Descriptor tags are @comma@ prefixed.
descriptor_tags :: Ascii -> Ascii
descriptor_tags = ByteString.Char8.drop 1
