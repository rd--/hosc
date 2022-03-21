-- | Parser for Osc datum.
module Sound.Osc.Datum.Parse where

import Data.Maybe {- base -}

import Sound.Osc.Datum {- hosc -}

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
  let reads_exact s = case reads s of { [(r,"")] -> Just r; _ -> Nothing }
  in case ty of
       'i' -> fmap Int32 . reads_exact
       'h' -> fmap Int64 . reads_exact
       'f' -> fmap Float . reads_exact
       'd' -> fmap Double . reads_exact
       's' -> fmap (AsciiString . ascii) . reads_exact
       'b' -> fmap (Blob . blob_pack) . reads_exact
       't' -> error "parse_datum: timestamp not implemented"
       'm' -> fmap midi . reads_exact
       _ -> error "parse_datum: unknown type"

-- | Erroring variant of 'parse_datum'.
parse_datum_err :: DatumType -> String -> Datum
parse_datum_err ty = fromMaybe (error "parse_datum") . parse_datum ty
