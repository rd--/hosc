-- | Data type for OSC datum.
module Sound.OSC.Datum where

import Data.Int {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Numeric {- base -}
import Text.Printf {- base -}
import Text.Read {- base -}

import qualified Data.ByteString.Lazy as Lazy {- bytestring -}
import qualified Data.ByteString.Char8 as Char8 {- bytestring -}

import qualified Sound.OSC.Time as Time {- hosc -}

-- * Datum

-- | Type enumerating Datum categories.
type Datum_Type = Char

-- | Type for ASCII strings (strict 'Char'8 'Char8.ByteString').
type ASCII = Char8.ByteString

-- | Type-specialised 'Char8.pack'.
ascii :: String -> ASCII
ascii = Char8.pack

-- | Type-specialised 'Char8.unpack'.
ascii_to_string :: ASCII -> String
ascii_to_string = Char8.unpack

-- | Type for 'Word8' arrays, these are stored with an 'Int32' length prefix.
type BLOB = Lazy.ByteString

-- | Type-specialised 'Lazy.pack'.
blob_pack ::  [Word8] -> BLOB
blob_pack = Lazy.pack

-- | Type-specialised 'Lazy.unpack'.
blob_unpack :: BLOB -> [Word8]
blob_unpack = Lazy.unpack

-- | Four-byte midi message: port-id, status-byte, data, data.
data MIDI = MIDI !Word8 !Word8 !Word8 !Word8
    deriving (Eq,Show,Read)

-- | The basic elements of OSC messages.
data Datum = Int32 {d_int32 :: !Int32}
           | Int64 {d_int64 :: !Int64}
           | Float {d_float :: !Float}
           | Double {d_double :: !Double}
           | ASCII_String {d_ascii_string :: !ASCII}
           | Blob {d_blob :: !BLOB}
           | TimeStamp {d_timestamp :: !Time.Time} -- ie. NTPr
           | Midi {d_midi :: !MIDI}
             deriving (Eq,Read,Show)

-- * Datum types

-- | List of required data types (tag,name).
osc_types_required :: [(Datum_Type,String)]
osc_types_required =
    [('i',"Int32")
    ,('f',"Float")
    ,('s',"ASCII_String") -- ASCII
    ,('b',"ByteArray") -- Blob
    ]

-- | List of optional data types (tag,name).
osc_types_optional :: [(Datum_Type,String)]
osc_types_optional =
    [('h',"Int64")
    ,('t',"TimeStamp")
    ,('d',"Double")
    -- ,('S',"Symbol")
    -- ,('c',"ASCII_Character")
    -- ,('r',"RGBA")
    ,('m',"MIDI")
    -- ,('T',"True")
    -- ,('F',"False")
    -- ,('N',"Nil")
    -- ,('I',"Infinitum")
    -- ,('[',"Array_Begin")
    -- ,(']',"Array_End")
    ]

-- | List of all data types (tag,name).
osc_types :: [(Datum_Type,String)]
osc_types = osc_types_required ++ osc_types_optional

-- | Lookup name of type.
osc_type_name :: Datum_Type -> Maybe String
osc_type_name c = lookup c osc_types

-- | Erroring variant.
osc_type_name_err :: Datum_Type -> String
osc_type_name_err = fromMaybe (error "osc_type_name") . osc_type_name

-- | Single character identifier of an OSC datum.
datum_tag :: Datum -> Datum_Type
datum_tag d =
    case d of
      Int32 _ -> 'i'
      Int64 _ -> 'h'
      Float _ -> 'f'
      Double _ -> 'd'
      ASCII_String _ -> 's'
      Blob _ -> 'b'
      TimeStamp _ -> 't'
      Midi _ -> 'm'

-- | Type and name of 'Datum'.
datum_type_name :: Datum -> (Datum_Type,String)
datum_type_name d = let c = datum_tag d in (c,osc_type_name_err c)

-- * Generalised element access

-- | 'Datum' as 'Integral' if Int32 or Int64.
--
-- > let d = [Int32 5,Int64 5,Float 5.5,Double 5.5]
-- > map datum_integral d == [Just (5::Int),Just 5,Nothing,Nothing]
datum_integral :: Integral i => Datum -> Maybe i
datum_integral d =
    case d of
      Int32 x -> Just (fromIntegral x)
      Int64 x -> Just (fromIntegral x)
      _ -> Nothing

-- | 'Datum' as 'Floating' if Int32, Int64, Float, Double or TimeStamp.
--
-- > let d = [Int32 5,Int64 5,Float 5,Double 5,TimeStamp 5]
-- > mapMaybe datum_floating d == replicate 5 (5::Double)
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

-- | Type generalised 'Int32'.
--
-- > int32 (1::Int32) == int32 (1::Integer)
-- > d_int32 (int32 (maxBound::Int32)) == maxBound
-- > int32 (((2::Int) ^ (64::Int))::Int) == Int32 0
int32 :: Integral n => n -> Datum
int32 = Int32 . fromIntegral

-- | Type generalised Int64.
--
-- > int64 (1::Int32) == int64 (1::Integer)
-- > d_int64 (int64 (maxBound::Int64)) == maxBound
int64 :: Integral n => n -> Datum
int64 = Int64 . fromIntegral

-- | Type generalised Float.
--
-- > float (1::Int) == float (1::Double)
-- > floatRange (undefined::Float) == (-125,128)
-- > isInfinite (d_float (float (encodeFloat 1 256 :: Double))) == True
float :: Real n => n -> Datum
float = Float . realToFrac

-- | Type generalised Double.
--
-- > double (1::Int) == double (1::Double)
-- > double (encodeFloat 1 256 :: Double) == Double 1.157920892373162e77
double :: Real n => n -> Datum
double = Double . realToFrac

-- | 'ASCII_String' of 'Char8.pack'.
--
-- > string "string" == ASCII_String (Char8.pack "string")
string :: String -> Datum
string = ASCII_String . Char8.pack

-- | Four-tuple variant of 'Midi' '.' 'MIDI'.
--
-- > midi (0,0,0,0) == Midi (MIDI 0 0 0 0)
midi :: (Word8,Word8,Word8,Word8) -> Datum
midi (p,q,r,s) = Midi (MIDI p q r s)

-- * Descriptor

-- | Message argument types are given by a descriptor.
--
-- > descriptor [Int32 1,Float 1,string "1"] == ascii ",ifs"
descriptor :: [Datum] -> ASCII
descriptor l = Char8.pack (',' : map datum_tag l)

-- | Descriptor tags are @comma@ prefixed.
descriptor_tags :: ASCII -> ASCII
descriptor_tags = Char8.drop 1

-- * Pretty printing

-- | Perhaps a precision value for floating point numbers.
type FP_Precision = Maybe Int

-- | Variant of 'showFFloat' that deletes trailing zeros.
--
-- > map (floatPP (Just 4)) [1,pi] == ["1.0","3.1416"]
floatPP :: RealFloat n => FP_Precision -> n -> String
floatPP p n =
    let s = showFFloat p n ""
        s' = dropWhile (== '0') (reverse s)
    in case s' of
         '.':_ -> reverse ('0' : s')
         _ -> reverse s'

-- | Pretty printer for 'Time'.
--
-- > timePP (Just 4) (1/3) == "0.3333"
timePP :: FP_Precision -> Time.Time -> String
timePP = floatPP

-- | Pretty printer for vectors.
--
-- > vecPP [1::Int,2,3] == "<1,2,3>"
vecPP :: Show a => [a] -> String
vecPP v = '<' : intercalate "," (map show v) ++ ">"

-- | Pretty printer for blobs, two-digit zero-padded hexadecimal.
blobPP :: BLOB -> String
blobPP = unwords . map (printf "%02X") . Lazy.unpack

{- | Pretty printer for 'Datum'.

> let d = [Int32 1,Float 1.2,string "str",midi (0,0x90,0x40,0x60)]
> map (datumPP (Just 5)) d ==  ["1","1.2","\"str\"","<0,144,64,96>"]

-}
datumPP :: FP_Precision -> Datum -> String
datumPP p d =
    case d of
      Int32 n -> show n
      Int64 n -> show n
      Float n -> floatPP p n
      Double n -> floatPP p n
      ASCII_String s -> show (Char8.unpack s)
      Blob s -> blobPP s
      TimeStamp t -> timePP p t
      Midi (MIDI b1 b2 b3 b4) -> vecPP [b1,b2,b3,b4]

-- | Variant of 'datumPP' that appends the 'datum_type_name'.
datum_pp_typed :: FP_Precision -> Datum -> String
datum_pp_typed fp d = datumPP fp d ++ ":" ++ snd (datum_type_name d)

-- * Parser

-- | Given 'Datum_Type' attempt to parse 'Datum' at 'String'.
--
-- > parse_datum 'i' "42" == Just (Int32 42)
-- > parse_datum 'h' "42" == Just (Int64 42)
-- > parse_datum 'f' "3.14159" == Just (Float 3.14159)
-- > parse_datum 'd' "3.14159" == Just (Double 3.14159)
-- > parse_datum 's' "\"pi\"" == Just (string "pi")
-- > parse_datum 'b' "[112,105]" == Just (Blob (blob_pack [112,105]))
-- > parse_datum 'm' "(0,144,60,90)" == Just (midi (0,144,60,90))
parse_datum :: Datum_Type -> String -> Maybe Datum
parse_datum ty =
    case ty of
      'i' -> fmap Int32 . readMaybe
      'h' -> fmap Int64 . readMaybe
      'f' -> fmap Float . readMaybe
      'd' -> fmap Double . readMaybe
      's' -> fmap (ASCII_String . Char8.pack) . readMaybe
      'b' -> fmap (Blob . blob_pack) . readMaybe
      't' -> error "parse_datum: timestamp not implemented"
      'm' -> fmap midi . readMaybe
      _ -> error "parse_datum: unknown type"

-- | Erroring variant of 'parse_datum'.
parse_datum_err :: Datum_Type -> String -> Datum
parse_datum_err ty = fromMaybe (error "parse_datum") . parse_datum ty
