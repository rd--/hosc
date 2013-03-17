-- | Datum normalisation.
module Sound.OSC.Normalise where

import Sound.OSC.Type

-- | A normalised 'Message' has only 'Int' and 'Double' numerical values.
--
-- > let m = message "/m" [Int 0,Float 0]
-- > in normalise_message m == message "/m" [Int 0,Double 0]
normalise_message :: Message -> Message
normalise_message = message_coerce f_to_d

-- | A normalised 'Bundle' has only 'Int' and 'Double' numerical values.
normalise_bundle :: Bundle -> Bundle
normalise_bundle = bundle_coerce f_to_d

-- * Coercion

-- | Map a normalising function over datum at an OSC 'Message'.
message_coerce :: (Datum -> Datum) -> Message -> Message
message_coerce f (Message s xs) = Message s (map f xs)

-- | Map a normalising function over datum at an OSC 'Bundle'.
bundle_coerce :: (Datum -> Datum) -> Bundle -> Bundle
bundle_coerce f (Bundle t xs) = Bundle t (map (message_coerce f) xs)

-- | Coerce Float to Double.
--
-- > f_to_d (Float 1) == Double 1
f_to_d :: Datum -> Datum
f_to_d d = case d of {Float n -> Double (realToFrac n);_ -> d}

-- | Coerce Int and Float to Double.
if_to_d :: Datum -> Datum
if_to_d d =
    case d of
      Int n -> Double (fromIntegral n)
      Float n -> Double (realToFrac n)
      _ -> d

-- | Coerce Float and Double to Int.
fd_to_i :: Datum -> Datum
fd_to_i d =
    case d of
      Float n -> Int (round n)
      Double n -> Int (round n)
      _ -> d

