-- | Parser for OSC datum.
module Sound.OSC.Datum.Parse where

import Data.Maybe {- base -}
import Text.Read {- base -}

import Sound.OSC.Datum {- hosc -}

-- | Given 'DatumType' attempt to parse 'Datum' at 'String'.
--
-- > parse_datum 'i' "42" == Just (Int32 42)
-- > parse_datum 'h' "42" == Just (Int64 42)
-- > parse_datum 'f' "3.14159" == Just (Float 3.14159)
-- > parse_datum 'd' "3.14159" == Just (Double 3.14159)
-- > parse_datum 's' "\"pi\"" == Just (string "pi")
-- > parse_datum 'b' "[112,105]" == Just (Blob (blob_pack [112,105]))
-- > parse_datum 'm' "(0,144,60,90)" == Just (midi (0,144,60,90))
parse_datum :: DatumType -> String -> Maybe Datum
parse_datum ty =
    case ty of
      'i' -> fmap Int32 . readMaybe
      'h' -> fmap Int64 . readMaybe
      'f' -> fmap Float . readMaybe
      'd' -> fmap Double . readMaybe
      's' -> fmap (Ascii_String . ascii) . readMaybe
      'b' -> fmap (Blob . blob_pack) . readMaybe
      't' -> error "parse_datum: timestamp not implemented"
      'm' -> fmap midi . readMaybe
      _ -> error "parse_datum: unknown type"

-- | Erroring variant of 'parse_datum'.
parse_datum_err :: DatumType -> String -> Datum
parse_datum_err ty = fromMaybe (error "parse_datum") . parse_datum ty
