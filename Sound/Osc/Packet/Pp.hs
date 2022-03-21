-- | Pretty printing for Osc packets
module Sound.Osc.Packet.Pp where

import Data.List {- base -}

import Sound.Osc.Datum.Pp {- hosc -}
import Sound.Osc.Packet {- hosc -}

{- | Pretty printer for 'Message'.

> messagePp Nothing (Message "/m" [int32 0,float 1.0,string "s",midi (1,2,3,4),blob [1,2,3]])
-}
messagePp :: FpPrecision -> Message -> String
messagePp p (Message a d) = let d' = map (datumPp p) d in unwords (a : d')

-- | Pretty printer for 'Bundle'.
bundlePp :: FpPrecision -> Bundle -> String
bundlePp p (Bundle t m) = let m' = intersperse ";" (map (messagePp p) m) in unwords (timePp p t : m')

-- | Pretty printer for 'Packet'.
packetPp :: FpPrecision -> Packet -> String
packetPp p pkt =
    case pkt of
      Packet_Message m -> messagePp p m
      Packet_Bundle b -> bundlePp p b
