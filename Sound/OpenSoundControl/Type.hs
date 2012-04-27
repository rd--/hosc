-- | Alegbraic data types for OSC datum and packets.
module Sound.OpenSoundControl.Type where

import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Word
import Sound.OpenSoundControl.Time

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
data Message = Message Address_Pattern [Datum]
               deriving (Eq,Read,Show)

-- | An OSC bundle.
data Bundle = Bundle Time [Message]
              deriving (Eq,Read,Show)

-- | An OSC 'Packet' is either a 'Message' or a 'Bundle'.
type Packet = Either Message Bundle

-- | OSC 'Bundle's can be ordered (time ascending).
instance Ord Bundle where
    compare (Bundle a _) (Bundle b _) = compare a b

-- | Single character identifier of an OSC datum.
tag :: Datum -> Char
tag dt =
    case dt of
      Int _ -> 'i'
      Float _ -> 'f'
      Double _ -> 'd'
      String _ -> 's'
      Blob _ -> 'b'
      TimeStamp _ -> 't'
      Midi _ -> 'm'

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

-- * Datum

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

-- | A 'fromJust' variant of 'datum_r'.
--
-- > map datum_real' [Int 5,Float 5] == [5,5]
datum_real' :: Datum -> Double
datum_real' = fromJust . datum_real

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
-- > map datum_int' [Int 5,Float 5.5] == [5,5]
datum_int' :: Integral i => Datum -> i
datum_int' = fromJust . datum_int

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
-- > map datum_string' [String "5",Blob (B.pack [53])] == ["5","5"]
datum_string' :: Datum -> String
datum_string' = fromJust . datum_string

-- | Does the OSC 'Message' have the specified 'Address_Pattern'.
message_has_address :: Address_Pattern -> Message -> Bool
message_has_address x (Message y _) = x == y

-- | Does the OSC 'Message' have the specified 'Address_Pattern'.
bundle_has_address :: Address_Pattern -> Bundle -> Bool
bundle_has_address x b =
    case b of
      Bundle _ (m:_) -> message_has_address x m
      _ -> error "bundle_has_address: empty bundle?"

-- | Does 'Packet' have the specified 'Address_Pattern'.
packet_has_address :: Address_Pattern -> Packet -> Bool
packet_has_address x p =
    case p of
      Left m -> message_has_address x m
      Right b -> bundle_has_address x b

packet_to_message :: Packet -> Message
packet_to_message p =
    case p of
      Left m -> m
      Right (Bundle _ (m:_)) -> m
      Right _ -> error "packet_to_message: empty bundle?"

packet_to_bundle :: Packet -> Bundle
packet_to_bundle p =
    case p of
      Left m -> Bundle immediately [m]
      Right b -> b
