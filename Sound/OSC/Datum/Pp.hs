-- | Pretty printing for Osc datum.
module Sound.OSC.Datum.Pp where

import Data.Char {- base -}
import Data.List {- base -}
import Numeric {- base -}
import Text.Printf {- base -}

import Sound.OSC.Datum {- hosc -}

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
timePP :: FP_Precision -> Time -> String
timePP = floatPP

-- | Pretty printer for vectors.
--
-- > vecPP show [1::Int,2,3] == "<1,2,3>"
vecPP :: (a -> String) -> [a] -> String
vecPP f v = '<' : intercalate "," (map f v) ++ ">"

-- | Pretty printer for blobs, two-digit zero-padded hexadecimal.
blobPP :: Blob -> String
blobPP = ('B':) . vecPP (printf "%02X") . blob_unpack

-- | Print strings in double quotes iff they contain white space.
stringPP :: String -> String
stringPP x = if any isSpace x then show x else x

{- | Pretty printer for 'Datum'.

> let d = [Int32 1,Float 1.2,string "str",midi (0,0x90,0x40,0x60),blob [12,16]]
> map (datumPP (Just 5)) d==  ["1","1.2","str","M<0,144,64,96>","B<0C,10>"]

-}
datumPP :: FP_Precision -> Datum -> String
datumPP p d =
    case d of
      Int32 n -> show n
      Int64 n -> show n
      Float n -> floatPP p n
      Double n -> floatPP p n
      Ascii_String s -> stringPP (ascii_to_string s)
      Blob s -> blobPP s
      TimeStamp t -> timePP p t
      Midi (MidiData b1 b2 b3 b4) -> 'M': vecPP show [b1,b2,b3,b4]

-- | Variant of 'datumPP' that appends the 'datum_type_name'.
datum_pp_typed :: FP_Precision -> Datum -> String
datum_pp_typed fp d = datumPP fp d ++ ":" ++ snd (datum_type_name d)
