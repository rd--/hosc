-- | Pretty printing for Osc datum.
module Sound.Osc.Datum.Pp where

import Data.Char {- base -}
import Data.List {- base -}
import Numeric {- base -}
import Text.Printf {- base -}

import Sound.Osc.Datum {- hosc -}

-- | Perhaps a precision value for floating point numbers.
type FpPrecision = Maybe Int

{- | Variant of 'showFFloat' that deletes trailing zeros.

> map (floatPp (Just 4)) [1,pi] == ["1.0","3.1416"]
-}
floatPp :: RealFloat n => FpPrecision -> n -> String
floatPp p n =
    let s = showFFloat p n ""
        s' = dropWhile (== '0') (reverse s)
    in case s' of
         '.':_ -> reverse ('0' : s')
         _ -> reverse s'

{- | Pretty printer for 'Time'.

> timePp (Just 4) (1/3) == "0.3333"
-}
timePp :: FpPrecision -> Time -> String
timePp = floatPp

{- | Pretty printer for vectors.

> vecPp show [1::Int,2,3] == "<1,2,3>"
-}
vecPp :: (a -> String) -> [a] -> String
vecPp f v = '<' : concat (intersperse "," (map f v)) ++ ">"

{- | Pretty printer for blobs, two-digit zero-padded hexadecimal.

Hugs has no printf instance for Word8 and no %X directive.

> blobPp (blob_pack [0, 63, 64, 127, 128, 255]) == "B<00,3f,40,7f,80,ff>"
-}
blobPp :: Blob -> String
blobPp = ('B':) . vecPp (printf "%02x") . blob_unpack_int

-- | Print strings in double quotes iff they contain white space.
stringPp :: String -> String
stringPp x = if any isSpace x then show x else x

{- | Pretty printer for 'Datum'.

> let d = [Int32 1,Float 1.2,string "str",midi (0,0x90,0x40,0x60),blob [12,16]]
> map (datumPp (Just 5)) d==  ["1","1.2","str","M<0,144,64,96>","B<0c,10>"]

-}
datumPp :: FpPrecision -> Datum -> String
datumPp p d =
    case d of
      Int32 n -> show n
      Int64 n -> show n
      Float n -> floatPp p n
      Double n -> floatPp p n
      AsciiString s -> stringPp (ascii_to_string s)
      Blob s -> blobPp s
      TimeStamp t -> timePp p t
      Midi (MidiData b1 b2 b3 b4) -> 'M': vecPp show [b1,b2,b3,b4]

-- | Variant of 'datumPp' that appends the 'datum_type_name'.
datum_pp_typed :: FpPrecision -> Datum -> String
datum_pp_typed fp d = datumPp fp d ++ ":" ++ snd (datum_type_name d)
