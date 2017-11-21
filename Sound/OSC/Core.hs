{- | Composite of non-transport related modules.

Provides the 'Datum', 'Message', 'Bundle' and 'Packet' types,
and the 'OSC' and 'Coding' type-classes.

The basic constructors are 'message' and 'bundle', the basic coding
functions are 'encodePacket' and 'decodePacket'.

> import Sound.OSC.Core {- hosc -}

> let o = bundle immediately [message "/g_free" [Int32 0]]
> let e = encodeBundle o
> decodeBundle e == o

-}
module Sound.OSC.Core (module M) where

import Sound.OSC.Coding as M
import Sound.OSC.Datum as M
import Sound.OSC.Packet as M
import Sound.OSC.Packet.Class as M
import Sound.OSC.Time as M
import Sound.OSC.Wait as M
