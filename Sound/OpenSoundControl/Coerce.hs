-- | OSC packet coercion and normalization.
module Sound.OpenSoundControl.Coerce where

import Sound.OpenSoundControl.OSC

-- | Map a normalizing function over datum at an osc packet.
coerce :: (Datum -> Datum) -> OSC -> OSC
coerce f (Message s xs) = Message s (map f xs)
coerce f (Bundle t xs) = Bundle t (map (coerce f) xs)

-- | Coerce Float to Double.
f_to_d :: Datum -> Datum
f_to_d (Float n) = Double n
f_to_d x = x

-- | Coerce Int and Float to Double.
if_to_d :: Datum -> Datum
if_to_d (Int n) = Double (fromIntegral n)
if_to_d (Float n) = Double n
if_to_d x = x

-- | Coerce Float and Double to Int.
fd_to_i :: Datum -> Datum
fd_to_i (Float n) = Int (round n)
fd_to_i (Double n) = Int (round n)
fd_to_i x = x

-- | A normalized osc packet has only Int and Double numerical values.
normalize :: OSC -> OSC
normalize = coerce f_to_d
