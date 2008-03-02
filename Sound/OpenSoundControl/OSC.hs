module Sound.OpenSoundControl.OSC ( OSC(..)
                                  , message, bundle
                                  , address, arguments
                                  , timestamp, messages
                                  , Datum(..)
                                  , int, float, double, string, blob
                                  , encodeOSC
                                  , encodeOSC_NTP
                                  , decodeOSC ) where

import Sound.OpenSoundControl.Time (ntpr_ntp)
import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.Cast (str_cstr)

import Data.Word (Word8)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B

-- | The basic elements of OSC messages.
data Datum = Int Int
           | Float Double
           | Double Double
           | String String
           | Blob [Word8]
             deriving (Eq, Show)

-- | Construct OSC int datum.
int :: Int -> Datum
int = Int

-- | Construct OSC float datum.
float :: Double -> Datum
float = Float

-- | Construct OSC double datum.
double :: Double -> Datum
double = Double

-- | Construct OSC string datum.
string :: String -> Datum
string = String

-- | Construct OSC blob datum.
blob :: [Word8] -> Datum
blob = Blob

-- | An OSC packet.
data OSC = Message String [Datum]
         | Bundle Double [OSC]
           deriving (Eq, Show)

-- | OSC message constructor
message :: String -> [Datum] -> OSC
message = Message

-- | OSC bundle constructor
bundle :: Double -> [OSC] -> OSC
bundle = Bundle

-- | Retrieve the address of an OSC message,
--   or Nothing for a bundle.
address :: OSC -> Maybe String
address (Message s _) = Just s
address (Bundle _ _) = Nothing

-- | Retrieve the arguments of an OSC message,
--   or Nothing for a bundle.
arguments :: OSC -> Maybe [Datum]
arguments (Message _ a) = Just a
arguments (Bundle _ _) = Nothing

-- | Retrieve the timestamp of an OSC bundle,
--   or Nothing for a message.
timestamp :: OSC -> Maybe Double
timestamp (Bundle t _) = Just t
timestamp (Message _ _) = Nothing

-- | Retrieve the messages in an OSC bundle,
--   or Nothing for a message.
messages :: OSC -> Maybe [OSC]
messages (Bundle _ m) = Just m
messages (Message _ _) = Nothing

instance Ord OSC where
    compare (Bundle a _) (Bundle b _) = compare a b
    compare _            _            = EQ

-- | OSC types have single character identifiers.
tag :: Datum -> Char
tag (Int _)    = 'i'
tag (Float _)  = 'f'
tag (Double _) = 'd'
tag (String _) = 's'
tag (Blob _)   = 'b'

-- | Command argument types are given by a descriptor.
descriptor :: [Datum] -> Datum
descriptor l = String (',' : map tag l)

-- | The number of bytes required to align an OSC value.
align :: Int -> Int
align n = mod (-n) 4

-- | Align a byte string if required.
extend :: a -> [a] -> [a]
extend p s = s ++ replicate (align (length s)) p

-- | Encode an OSC datum.
encodeDatum :: Datum -> B.ByteString
encodeDatum (Int i)    = encode_i32 i
encodeDatum (Float f)  = encode_f32 f
encodeDatum (Double d) = encode_f64 d
encodeDatum (String s) = B.pack (extend 0 (str_cstr s))
encodeDatum (Blob b)   = B.concat [encode_i32 (length b), B.pack (extend 0 b)]

-- | Encode an OSC packet (NTP epoch).
encodeOSC_NTP :: OSC -> B.ByteString
encodeOSC_NTP (Message c l) = B.concat [ encodeDatum (String c)
                                       , encodeDatum (descriptor l)
                                       , B.concat (map encodeDatum l) ]
encodeOSC_NTP (Bundle t l) = B.concat [ encodeDatum (String "#bundle")
                                      , encode_u64 (ntpr_ntp t)
                                      , B.concat (map f l) ]
    where f = encodeDatum . Blob . B.unpack . encodeOSC

-- | Encode an OSC packet.
encodeOSC :: OSC -> B.ByteString
encodeOSC (Message c l) = encodeOSC_NTP (Message c l)
encodeOSC (Bundle t l) = encodeOSC_NTP (Bundle (t + n) l)
    where n = (70 * 365 + 17) * 24 * 60 * 60

-- | The plain byte count of an OSC value.
size :: Char -> B.ByteString -> Int
size 'i' _ = 4
size 'f' _ = 4
size 'd' _ = 8
size 's' b = fromIntegral (fromMaybe
                           (error ("no terminating zero found in " ++ show b))
                           (B.elemIndex 0 b))
size 'b' b = decode_i32 (B.take 4 b)
size _   _ = error "illegal osc type"

-- | The storage byte count of an OSC value.
storage :: Char -> B.ByteString -> Int
storage 's'  b = n + align n where n = size 's' b + 1
storage 'b'  b = n + align n + 4 where n = size 'b' b
storage c    _ = size c B.empty

decodeDatum :: Char -> B.ByteString -> Datum
decodeDatum 'i' b = Int (decode_i32 b)
decodeDatum 'f' b = Float (decode_f32 b)
decodeDatum 'd' b = Double (decode_f64 b)
decodeDatum 's' b = String (decode_str (take' n b)) where n = size 's' b
decodeDatum 'b' b = Blob (B.unpack (take' n (B.drop 4 b))) where n = size 'b' b
decodeDatum _   _ = error "illegal osc type"

-- | Trivial utility.
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- | Decode data given a type descriptor string.
decodeData :: [Char] -> B.ByteString -> [Datum]
decodeData cs b =
   zipWith decodeDatum cs $ snd $
   mapAccumL (\bRest c -> swap (B.splitAt (fromIntegral (storage c bRest)) bRest)) b cs

-- | Decode an OSC packet.
decodeOSC :: B.ByteString -> OSC
decodeOSC b = Message cmd arg
    where n            = storage 's' b
          (String cmd) = decodeDatum 's' b
          m            = storage 's' (drop' n b)
          (String dsc) = decodeDatum 's' (drop' n b)
          arg          = decodeData (drop 1 dsc) (drop' (n + m) b)

take' :: Int -> B.ByteString -> B.ByteString
take' n b = B.take (fromIntegral n) b

drop' :: Int -> B.ByteString -> B.ByteString
drop' n b = B.drop (fromIntegral n) b
