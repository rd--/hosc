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

-- | An OSC packet.
data OSC = Message String [Datum]
         | Bundle Time [OSC]
           deriving (Eq,Read,Show)

-- | OSC bundles can be ordered (time ascending).  Bundles and
--   messages compare 'EQ'.
instance Ord OSC where
    compare (Bundle a _) (Bundle b _) = compare a b
    compare _ _ = EQ

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

-- | Bundle constructor. It is an 'error' if the 'OSC' list is empty.
bundle :: Time -> [OSC] -> OSC
bundle t xs =
    case xs of
      [] -> error "bundle: empty?"
      _ -> Bundle t xs

-- | Message constructor.  It is an 'error' if the address doesn't
-- conform to the OSC specification.
message :: String -> [Datum] -> OSC
message a xs =
    case a of
      '/':_ -> Message a xs
      _ -> error "message: ill-formed address"

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
