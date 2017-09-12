-- | Packet coercion.
module Sound.OSC.Packet.Coerce where

import Sound.OSC.Datum {- hosc -}
import Sound.OSC.Packet as O {- hosc -}

-- | Map a normalising function over datum at an OSC 'Message'.
message_coerce :: (Datum -> Datum) -> Message -> Message
message_coerce f (Message s xs) = Message s (map f xs)

-- | Map a normalising function over datum at an OSC 'Bundle'.
bundle_coerce :: (Datum -> Datum) -> Bundle -> Bundle
bundle_coerce f (Bundle t xs) = Bundle t (map (message_coerce f) xs)
