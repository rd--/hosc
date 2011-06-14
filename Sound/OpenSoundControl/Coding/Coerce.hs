-- | OSC packet coercion and normalization.
module Sound.OpenSoundControl.Coding.Coerce where

import Sound.OpenSoundControl.Type

-- | Map a normalizing function over datum at an osc packet.
coerce :: (Datum -> Datum) -> OSC -> OSC
coerce f o =
    case o of
      Message s xs -> Message s (map f xs)
      Bundle t xs -> Bundle t (map (coerce f) xs)

-- | Coerce Float to Double.
f_to_d :: Datum -> Datum
f_to_d d =
    case d of
      Float n -> Double n
      _ -> d

-- | Coerce Int and Float to Double.
if_to_d :: Datum -> Datum
if_to_d d =
    case d of
      Int n -> Double (fromIntegral n)
      Float n -> Double n
      _ -> d

-- | Coerce Float and Double to Int.
fd_to_i :: Datum -> Datum
fd_to_i d =
    case d of
      Float n -> Int (round n)
      Double n -> Int (round n)
      _ -> d

-- | A normalized osc packet has only Int and Double numerical values.
normalize :: OSC -> OSC
normalize = coerce f_to_d
