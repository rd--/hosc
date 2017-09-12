-- | Unpack 'Datum' lists into tuples.
module Sound.OSC.Datum.Unpack where

import Data.Int {- base -}

import Sound.OSC.Datum {- hosc -}

-- * S = strict

unpackS_sf :: [Datum] -> Maybe (String,Float)
unpackS_sf dat =
    case dat of
      [ASCII_String d1,Float d2] -> Just (ascii_to_string d1,d2)
      _ -> Nothing

unpackS_if :: [Datum] -> Maybe (Int32,Float)
unpackS_if dat =
    case dat of
      [Int32 d1,Float d2] -> Just (d1,d2)
      _ -> Nothing

-- * C = coerce

unpackC_sf :: [Datum] -> Maybe (String,Double)
unpackC_sf dat =
    case dat of
      [ASCII_String d1,d2] ->
        case datum_floating d2 of
          Just d2' -> Just (ascii_to_string d1,d2')
          Nothing -> Nothing
      _ -> Nothing

unpackC_if :: [Datum] -> Maybe (Int,Double)
unpackC_if dat =
    case dat of
      [d1,d2] ->
          case (datum_integral d1,datum_floating d2) of
            (Just d1',Just d2') -> Just (d1',d2')
            _ -> Nothing
      _ -> Nothing
