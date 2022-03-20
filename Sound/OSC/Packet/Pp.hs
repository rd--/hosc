-- | Pretty printing for Osc packets
module Sound.OSC.Packet.Pp where

import Data.List {- base -}

import Sound.OSC.Datum.Pp {- hosc -}
import Sound.OSC.Packet {- hosc -}

{- | Pretty printer for 'Message'.

> messagePP Nothing (Message "/m" [int32 0,float 1.0,string "s",midi (1,2,3,4),blob [1,2,3]])
-}
messagePP :: FP_Precision -> Message -> String
messagePP p (Message a d) = let d' = map (datumPP p) d in unwords (a : d')

-- | Pretty printer for 'Bundle'.
bundlePP :: FP_Precision -> Bundle -> String
bundlePP p (Bundle t m) = let m' = intersperse ";" (map (messagePP p) m) in unwords (timePP p t : m')

-- | Pretty printer for 'Packet'.
packetPP :: FP_Precision -> Packet -> String
packetPP p pkt =
    case pkt of
      Packet_Message m -> messagePP p m
      Packet_Bundle b -> bundlePP p b
