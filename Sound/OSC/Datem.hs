-- | A class for translating to and from 'Datum'.
module Sound.OSC.Datem where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import Data.Int {- base -}
import Data.Maybe {- base -}

import Sound.OSC.Type

{- | Class for translating to and from 'Datum'.
     There are instances for the direct 'Datum' field types.

> d_put (1::Int32) == Int32 1
> d_put (1::Int64) == Int64 1
> d_put (1::Float) == Float 1
> d_put (1::Double) == Double 1
> d_put (C.pack "str") == ASCII_String (C.pack "str")
> d_put (B.pack [37,37]) == Blob (blob_pack [37,37])
> d_put (MIDI 0 0 0 0) == Midi (MIDI 0 0 0 0)

There are also instances for standard Haskell types.

> d_put (1::Int) == Int64 1
> d_put (1::Integer) == Int64 1

-}
class Datem a where
    d_put :: a -> Datum
    d_get :: Datum -> Maybe a

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

instance Datem C.ByteString where
    d_put = ASCII_String
    d_get d = case d of {ASCII_String x -> Just x;_ -> Nothing}

instance Datem B.ByteString where
    d_put = Blob
    d_get d = case d of {Blob x -> Just x;_ -> Nothing}

instance Datem MIDI where
    d_put = Midi
    d_get d = case d of {Midi x -> Just x;_ -> Nothing}

-- | Error variant of 'd_get'.
d_get_err :: Datem a => Datum -> a
d_get_err = fromMaybe (error "d_get") . d_get

