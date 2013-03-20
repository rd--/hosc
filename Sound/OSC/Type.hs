-- | Alegbraic data types for OSC datum and packets.
module Sound.OSC.Type where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.List {- base -}
import Data.Word {- base -}

-- * Time

-- | @NTP@ time in real-valued (fractional) form.
type Time = Double

-- | Constant indicating a bundle to be executed immediately.
immediately :: Time
immediately = 1 / 2^(32::Int)

-- * Datum

-- | Type enumerating Datum categories.
type Datum_Type = Char

-- | The basic elements of OSC messages.
data Datum = Int {d_int :: Int}
           | Float {d_float :: Float}
           | Double {d_double :: Double}
           | String {d_string :: String}
           | Blob {d_blob :: B.ByteString}
           | TimeStamp {d_timestamp :: Time}
           | Midi {d_midi :: (Word8,Word8,Word8,Word8)}
             deriving (Eq,Read,Show)

-- | Type generalised 'Int'.
--
-- > int (1::Int) == int (1::Integer)
-- > d_int (int (maxBound::Data.Int.Int32)) <= d_int (int (maxBound::Int))
-- > int (2 ^ 64 :: Integer) == int 0
int :: Integral n => n -> Datum
int = Int . fromIntegral

-- | Type generalised 'Float'.
--
-- > float (1::Int) == float (1::Double)
-- > floatRange (undefined::Float) == (-125,128)
-- > isInfinite (d_float (float (encodeFloat 1 256 :: Double))) == True
float :: Real n => n -> Datum
float = Float . realToFrac

-- | Type generalised 'Double'.
--
-- > double (1::Int) == double (1::Double)
-- > double (encodeFloat 1 256 :: Double) == double 1.157920892373162e77
double :: Real n => n -> Datum
double = Double . realToFrac

-- | 'Maybe' variant of 'd_int'.
--
-- > map datum_int [Int 1,Float 1] == [Just 1,Nothing]
datum_int :: Datum -> Maybe Int
datum_int d = case d of {Int x -> Just x;_ -> Nothing}

-- | 'Maybe' variant of 'd_float'.
datum_float :: Datum -> Maybe Float
datum_float d = case d of {Float x -> Just x;_ -> Nothing}

-- | 'Maybe' variant of 'd_double'.
datum_double :: Datum -> Maybe Double
datum_double d = case d of {Double x -> Just x;_ -> Nothing}

-- | 'Maybe' variant of 'd_string'.
datum_string :: Datum -> Maybe String
datum_string d = case d of {String x -> Just x;_ -> Nothing}

-- | 'Maybe' variant of 'd_blob'.
datum_blob :: Datum -> Maybe B.ByteString
datum_blob d = case d of {Blob x -> Just x;_ -> Nothing}

-- | 'Maybe' variant of 'd_timestamp'.
datum_timestamp :: Datum -> Maybe Time
datum_timestamp d = case d of {TimeStamp x -> Just x;_ -> Nothing}

-- | 'Maybe' variant of 'd_midi'.
datum_midi :: Datum -> Maybe (Word8,Word8,Word8,Word8)
datum_midi d = case d of {Midi x -> Just x;_ -> Nothing}

-- | 'Datum' as 'Integral' if 'Int', 'Float' or 'Double'.
--
-- > let d = [Int 5,Float 5.5,Double 5.5,String "5"]
-- > in map datum_integral d == [Just 5,Just 5,Just 5,Nothing]
datum_integral :: Integral i => Datum -> Maybe i
datum_integral d =
    case d of
      Int x -> Just (fromIntegral x)
      Float x -> Just (floor x)
      Double x -> Just (floor x)
      _ -> Nothing

-- | 'Datum' as 'Floating' if 'Int', 'Float' or 'Double'.
--
-- > let d = [Int 5,Float 5,Double 5,String "5"]
-- > in map datum_floating d == [Just 5,Just 5,Just 5,Nothing]
datum_floating :: Floating n => Datum -> Maybe n
datum_floating d =
    case d of
      Int n -> Just (fromIntegral n)
      Float n -> Just (realToFrac n)
      Double n -> Just (realToFrac n)
      _ -> Nothing

-- | 'Datum' as sequence of 'Int' if 'String', 'Blob' or 'Midi'.
--
-- > let d = [String "5",Blob (B.pack [53]),Midi (0x00,0x90,0x40,0x60)]
-- > in Data.Maybe.mapMaybe datum_sequence d == [[53],[53],[0,144,64,96]]
datum_sequence :: Datum -> Maybe [Int]
datum_sequence d =
    case d of
      String s -> Just (map fromEnum s)
      Blob s -> Just (map fromIntegral (B.unpack s))
      Midi (p,q,r,s) -> Just (map fromIntegral [p,q,r,s])
      _ -> Nothing

-- | Single character identifier of an OSC datum.
datum_tag :: Datum -> Datum_Type
datum_tag dt =
    case dt of
      Int _ -> 'i'
      Float _ -> 'f'
      Double _ -> 'd'
      String _ -> 's'
      Blob _ -> 'b'
      TimeStamp _ -> 't'
      Midi _ -> 'm'

-- * Message

-- | OSC address pattern.
type Address_Pattern = String

-- | An OSC message.
data Message = Message {messageAddress :: Address_Pattern
                       ,messageDatum :: [Datum]}
               deriving (Eq,Read,Show)

-- | 'Message' constructor.  It is an 'error' if the 'Address_Pattern'
-- doesn't conform to the OSC specification.
message :: Address_Pattern -> [Datum] -> Message
message a xs =
    case a of
      '/':_ -> Message a xs
      _ -> error "message: ill-formed address pattern"

-- * Bundle

-- | An OSC bundle.
data Bundle = Bundle {bundleTime :: Time
                     ,bundleMessages :: [Message]}
              deriving (Eq,Read,Show)

-- | OSC 'Bundle's can be ordered (time ascending).
instance Ord Bundle where
    compare (Bundle a _) (Bundle b _) = compare a b

-- | 'Bundle' constructor. It is an 'error' if the 'Message' list is
-- empty.
bundle :: Time -> [Message] -> Bundle
bundle t xs =
    case xs of
      [] -> error "bundle: empty?"
      _ -> Bundle t xs

-- * Packet

-- | An OSC 'Packet' is either a 'Message' or a 'Bundle'.
data Packet = Packet_Message {packetMessage :: Message}
            | Packet_Bundle {packetBundle :: Bundle}
              deriving (Eq,Read,Show)

-- | 'Packet_Bundle' '.' 'bundle'.
p_bundle :: Time -> [Message] -> Packet
p_bundle t = Packet_Bundle . bundle t

-- | 'Packet_Message' '.' 'message'.
p_message :: Address_Pattern -> [Datum] -> Packet
p_message a = Packet_Message . message a

-- | The 'Time' of 'Packet', if the 'Packet' is a 'Message' this is
-- 'immediately'.
packetTime :: Packet -> Time
packetTime = at_packet (const immediately) bundleTime

-- | Retrieve the set of 'Message's from a 'Packet'.
packetMessages :: Packet -> [Message]
packetMessages = at_packet return bundleMessages

-- | If 'Packet' is a 'Message' add 'immediately' timestamp, else 'id'.
packet_to_bundle :: Packet -> Bundle
packet_to_bundle = at_packet (\m -> Bundle immediately [m]) id

-- | If 'Packet' is a 'Message' or a 'Bundle' with an /immediate/ time
-- tag and with one element, return the 'Message', else 'Nothing'.
packet_to_message :: Packet -> Maybe Message
packet_to_message p =
    case p of
      Packet_Bundle b ->
          case b of
            Bundle t [m] -> if t == immediately then Just m else Nothing
            _ -> Nothing
      Packet_Message m -> Just m

-- | Is 'Packet' immediate, ie. a 'Bundle' with timestamp
-- 'immediately', or a plain Message.
packet_is_immediate :: Packet -> Bool
packet_is_immediate = (== immediately) . packetTime

-- | Variant of 'either' for 'Packet'.
at_packet :: (Message -> a) -> (Bundle -> a) -> Packet -> a
at_packet f g p =
    case p of
      Packet_Message m -> f m
      Packet_Bundle b -> g b

-- * Address Query

-- | Does 'Message' have the specified 'Address_Pattern'.
message_has_address :: Address_Pattern -> Message -> Bool
message_has_address x = (== x) . messageAddress

-- | Do any of the 'Message's at 'Bundle' have the specified
-- 'Address_Pattern'.
bundle_has_address :: Address_Pattern -> Bundle -> Bool
bundle_has_address x = any (message_has_address x) . bundleMessages

-- | Does 'Packet' have the specified 'Address_Pattern', ie.
-- 'message_has_address' or 'bundle_has_address'.
packet_has_address :: Address_Pattern -> Packet -> Bool
packet_has_address x =
    at_packet (message_has_address x)
              (bundle_has_address x)

-- * Pretty printing

-- | Pretty printer for 'Time'.
timePP :: Time -> String
timePP = (:) 'N' . show

-- | Pretty printer for 'Datum'.
--
-- > let d = [Float 1.2,String "str",Midi (0,0x90,0x40,0x60)]
-- > in map datumPP d ==  ["1.2","\"str\"","<0,144,64,96>"]
datumPP :: Datum -> String
datumPP d =
    case d of
      Int n -> show n
      Float n -> show n
      Double n -> show n
      String s -> show s
      Blob s -> show s
      TimeStamp t -> timePP t
      Midi (p,q,r,s) -> '<' : intercalate "," (map show [p,q,r,s]) ++ ">"

-- | Pretty printer for 'Message'.
messagePP :: Message -> String
messagePP (Message a d) = unwords ("#message" : a : map datumPP d)

-- | Pretty printer for 'Bundle'.
bundlePP :: Bundle -> String
bundlePP (Bundle t m) =
    let m' = intersperse ";" (map messagePP m)
    in unwords ("#bundle" : timePP t : m')

-- | Pretty printer for 'Packet'.
packetPP :: Packet -> String
packetPP p =
    case p of
      Packet_Message m -> messagePP m
      Packet_Bundle b -> bundlePP b

-- * Parser

-- | Variant of 'read'.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
    case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- | Given 'Datum_Type' attempt to parse 'Datum' at 'String'.
--
-- > parse_datum 'i' "42" == Just (Int 42)
-- > parse_datum 'f' "3.14159" == Just (Float 3.14159)
-- > parse_datum 'd' "3.14159" == Just (Double 3.14159)
-- > parse_datum 's' "\"pi\"" == Just (String "pi")
-- > parse_datum 'b' "pi" == Just (Blob (B.pack [112,105]))
-- > parse_datum 'm' "(0,144,60,90)" == Just (Midi (0,144,60,90))
parse_datum :: Datum_Type -> String -> Maybe Datum
parse_datum ty =
    case ty of
      'i' -> fmap Int . readMaybe
      'f' -> fmap Float . readMaybe
      'd' -> fmap Double . readMaybe
      's' -> fmap String . readMaybe
      'b' -> Just . Blob . B.pack . map (fromIntegral . fromEnum)
      't' -> error "parse_datum: timestamp"
      'm' -> fmap Midi . readMaybe
      _ -> error "parse_datum: type"
