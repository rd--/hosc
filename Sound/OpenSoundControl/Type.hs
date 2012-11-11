-- | Alegbraic data types for OSC datum and packets.
module Sound.OpenSoundControl.Type where

import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Data.Word
import Sound.OpenSoundControl.Time

-- | Type enumerating Datum categories.
type Datum_Type = Char

-- | The basic elements of OSC messages.
data Datum = Int Int
           | Float Double
           | Double Double
           | String String
           | Blob B.ByteString
           | TimeStamp Time
           | Midi (Word8,Word8,Word8,Word8)
             deriving (Eq,Read,Show)

-- | OSC address pattern.
type Address_Pattern = String

-- | An OSC message.
data Message = Message {messageAddress :: Address_Pattern
                       ,messageDatum :: [Datum]}
               deriving (Eq,Read,Show)

-- | An OSC bundle.
data Bundle = Bundle {bundleTime :: Time
                     ,bundleMessages :: [Message]}
              deriving (Eq,Read,Show)

-- | An OSC 'Packet' is either a 'Message' or a 'Bundle'.
data Packet = Packet_Message {packetMessage :: Message}
            | Packet_Bundle {packetBundle :: Bundle}
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

-- | 'Message' constructor.  It is an 'error' if the 'Address_Pattern'
-- doesn't conform to the OSC specification.
message :: Address_Pattern -> [Datum] -> Message
message a xs =
    case a of
      '/':_ -> Message a xs
      _ -> error "message: ill-formed address pattern"

-- | 'Packet_Bundle' '.' 'bundle'.
p_bundle :: Time -> [Message] -> Packet
p_bundle t = Packet_Bundle . bundle t

-- | 'Packet_Message' '.' 'message'.
p_message :: Address_Pattern -> [Datum] -> Packet
p_message a = Packet_Message . message a

-- * Datum

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

-- | 'Datum' as real number if 'Double', 'Float' or 'Int', else 'Nothing'.
--
-- > map datum_real [Int 5,Float 5,String "5"] == [Just 5,Just 5,Nothing]
datum_real :: Datum -> Maybe Double
datum_real d =
    case d of
      Double n -> Just n
      Float n -> Just n
      Int n -> Just (fromIntegral n)
      _ -> Nothing

-- | A 'fromJust' variant of 'datum_real'.
--
-- > map datum_real_err [Int 5,Float 5] == [5,5]
datum_real_err :: Datum -> Double
datum_real_err = fromJust . datum_real

-- | 'Datum' as integral number if 'Double', 'Float' or 'Int', else
-- 'Nothing'.
--
-- > map datum_int [Int 5,Float 5.5,String "5"] == [Just 5,Just 5,Nothing]
datum_int :: Integral i => Datum -> Maybe i
datum_int d =
    case d of
      Int x -> Just (fromIntegral x)
      Float x -> Just (floor x)
      Double x -> Just (floor x)
      _ -> Nothing

-- | A 'fromJust' variant of 'datum_int'.
--
-- > map datum_int_err [Int 5,Float 5.5] == [5,5]
datum_int_err :: Integral i => Datum -> i
datum_int_err = fromJust . datum_int

-- | 'Datum' as 'String' if 'String' or 'Blob', else 'Nothing'.
--
-- > map datum_string [String "5",Blob (B.pack [53])] == [Just "5",Just "5"]
datum_string :: Datum -> Maybe String
datum_string d =
    case d of
      Blob s -> Just (map (toEnum . fromIntegral) (B.unpack s))
      String s -> Just s
      _ -> Nothing

-- | A 'fromJust' variant of 'datum_string'.
--
-- > map datum_string_err [String "5",Blob (B.pack [53])] == ["5","5"]
datum_string_err :: Datum -> String
datum_string_err = fromJust . datum_string

-- * Address

-- | Does 'Message' have the specified 'Address_Pattern'.
message_has_address :: Address_Pattern -> Message -> Bool
message_has_address x = (== x) . messageAddress

-- | Do any of the 'Message's at 'Bundle' have the specified
-- 'Address_Pattern'.
bundle_has_address :: Address_Pattern -> Bundle -> Bool
bundle_has_address x = any (message_has_address x) . bundleMessages

-- * Packet

-- | Does 'Packet' have the specified 'Address_Pattern', ie.
-- 'message_has_address' or 'bundle_has_address'.
packet_has_address :: Address_Pattern -> Packet -> Bool
packet_has_address x =
    at_packet (message_has_address x)
              (bundle_has_address x)

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

-- * Pretty printing

-- | Pretty printer for 'Time'.
--
-- > map timePP [UTCr 0,NTPr 0,NTPi 0]
timePP :: Time -> String
timePP t =
    case t of
      UTCr n -> 'U' : show n
      NTPr n -> 'N' : show n
      NTPi i -> 'N' : show (ntpi_ntpr i)

-- | Pretty printer for 'Datum'.
--
-- > map datumPP [Float 1.2,String "str",Midi (0,0x90,0x40,0x60)]
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
