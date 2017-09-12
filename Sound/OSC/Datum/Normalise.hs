-- | Datum normalisation.
module Sound.OSC.Datum.Normalise where

import Sound.OSC.Datum {- hosc -}
import Sound.OSC.Packet as O {- hosc -}

-- | Lift 'O.Int32' to 'O.Int64' and 'O.Float' to 'O.Double'.
--
-- > map normalise_datum [Int32 1,Float 1] == [Int64 1,Double 1]
normalise_datum :: Datum -> Datum
normalise_datum d =
    case d of
      Int32 n -> Int64 (fromIntegral n)
      Float n -> Double (realToFrac n)
      _ -> d

-- | A normalised 'O.Message' has only 'O.Int64' and 'O.Double'
-- numerical values.
--
-- > let m = message "/m" [Int32 0,Float 0]
-- > in normalise_message m == message "/m" [Int64 0,Double 0]
normalise_message :: Message -> Message
normalise_message = message_coerce normalise_datum

-- | A normalised 'O.Bundle' has only 'O.Int64' and 'O.Double'
-- numerical values.
normalise_bundle :: Bundle -> Bundle
normalise_bundle = bundle_coerce normalise_datum

-- * Coercion

-- | Map a normalising function over datum at an OSC 'Message'.
message_coerce :: (Datum -> Datum) -> Message -> Message
message_coerce f (Message s xs) = Message s (map f xs)

-- | Map a normalising function over datum at an OSC 'Bundle'.
bundle_coerce :: (Datum -> Datum) -> Bundle -> Bundle
bundle_coerce f (Bundle t xs) = Bundle t (map (message_coerce f) xs)

-- * Promotion

-- | Coerce 'Int32', 'Int64' and 'Float' to 'Double'.
--
-- > map datum_promote [Int32 5,Float 5] == [Double 5,Double 5]
datum_promote :: Datum -> Datum
datum_promote d =
    case d of
      Int32 n -> Double (fromIntegral n)
      Int64 n -> Double (fromIntegral n)
      Float n -> Double (realToFrac n)
      _ -> d

-- | 'O.Datum' as 'O.Int64' if 'O.Int32', 'O.Int64', 'O.Float' or
-- 'O.Double'.
--
-- > let d = [Int32 5,Int64 5,Float 5.5,Double 5.5,string "5"]
-- > in map datum_floor d == [Int64 5,Int64 5,Int64 5,Int64 5,string "5"]
datum_floor :: Datum -> Datum
datum_floor d =
    case d of
      Int32 x -> Int64 (fromIntegral x)
      Float x -> Int64 (fromInteger (floor x))
      Double x -> Int64 (fromInteger (floor x))
      _ -> d

