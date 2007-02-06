module Sound.OpenSoundControl.OSC
   (OSC(..),
    Datum(..),
    Encodable, encode,
    Decodable, decode,
    ) where

import Sound.OpenSoundControl.Time (utc_ntp)
import Sound.OpenSoundControl.U8v

import Data.List (elemIndex, mapAccumL)
import Data.Maybe (fromMaybe)

data Datum = Int Int
           | Float Double
           | Double Double
           | String String
           | Blob [U8]
             deriving (Eq)

instance Show Datum where
    show (Int n)    = show n
    show (Float n)  = show n
    show (Double n) = show n
    show (String s) = show s
    show (Blob b)   = show b

data OSC = Message String [Datum]
         | Bundle Double [OSC]
           deriving (Eq, Show)

class Encodable a where
    encode :: a -> [U8]

class Decodable a where
    decode :: [U8] -> a

-- | OSC types have single character identifiers.
tag :: Datum -> Char
tag (Int _)    = 'i'
tag (Float _)  = 'f'
tag (Double _) = 'd'
tag (String _) = 's'
tag (Blob _)   = 'b'

-- | Command argument types are given by a descriptor.
descriptor :: [Datum] -> [U8]
descriptor l = encode (String $ ',' : map tag l)

-- | The number of bytes required to align an OSC value.
align :: Int -> Int
align n = mod (-n) 4

-- | Align a byte string if required.
extend :: a -> [a] -> [a]
extend p s = s ++ replicate (align (length s)) p

instance Encodable Datum where
    encode (Int i)    = i32_u8v i
    encode (Float f)  = f32_u8v (f64_f32 f)
    encode (Double d) = f64_u8v d
    encode (String s) = extend 0 (cstr_u8v s)
    encode (Blob b)   = i32_u8v (length b) ++ extend 0 b

instance Encodable OSC where
    encode (Message c l) = encode (String c) ++
                           descriptor l ++
                           concatMap encode l
    encode (Bundle t l) = encode (String "#bundle") ++
                          u64_u8v (utc_ntp t) ++
                          concatMap (encode . Blob . encode) l

-- | The plain byte count of an OSC value.
size :: Char -> [U8] -> Int
size 'i' _ = 4
size 'f' _ = 4
size 'd' _ = 8
size 's' b = fromMaybe (error ("no terminating zero found in " ++ show b))
                       (elemIndex 0 b)
size 'b' b = u8v_i32 (take 4 b)
size _   _ = error "illegal osc type"

-- | The storage byte count of an OSC value.
storage :: Char -> [U8] -> Int
storage 's'  b = n + align n where n = size 's' b + 1
storage 'b'  b = n + align n + 4 where n = size 'b' b
storage c    _ = size c []

decodeDatum :: Char -> [U8] -> Datum
decodeDatum 'i' b = Int $ u8v_i32 b
decodeDatum 'f' b = Float $ f32_f64 (u8v_f32 b)
decodeDatum 'd' b = Double $ u8v_f64 b
decodeDatum 's' b = String $ u8v_str $ take n b where n = size 's' b
decodeDatum 'b' b = Blob $ take n (drop 4 b) where n = size 'b' b
decodeDatum _   _ = error "illegal osc type"

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

decodeData :: [Char] -> [U8] -> [Datum]
decodeData cs b =
   zipWith decodeDatum cs $ snd $
   mapAccumL (\bRest c -> swap (splitAt (storage c bRest) bRest)) b cs

instance Decodable OSC where
    decode b = Message cmd arg
        where n            = storage 's' b
              (String cmd) = decodeDatum 's' b
              m            = storage 's' (drop n b)
              (String dsc) = decodeDatum 's' (drop n b)
              arg          = decodeData (drop 1 dsc) (drop (n + m) b)
