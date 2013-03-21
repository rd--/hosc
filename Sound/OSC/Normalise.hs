-- | Datum normalisation.
module Sound.OSC.Normalise where

import Sound.OSC.Type

-- | Coerce Float to Double.
--
-- > map normalise_datum [Int32 1,Float 1] == [Int64 1,Double 1]
normalise_datum :: Datum -> Datum
normalise_datum d =
    case d of
      Int32 n -> Int64 (fromIntegral n)
      Float n -> Double (realToFrac n)
      _ -> d

-- | A normalised 'Message' has only 'Int64' and 'Double' numerical values.
--
-- > let m = message "/m" [Int32 0,Float 0]
-- > in normalise_message m == message "/m" [Int64 0,Double 0]
normalise_message :: Message -> Message
normalise_message = message_coerce normalise_datum

-- | A normalised 'Bundle' has only 'Int64' and 'Double' numerical values.
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
-- > map datum_promote [Int 5,Float 5] == [Double 5,Double 5]
datum_promote :: Datum -> Datum
datum_promote d =
    case d of
      Int32 n -> Double (fromIntegral n)
      Int64 n -> Double (fromIntegral n)
      Float n -> Double (realToFrac n)
      _ -> d

