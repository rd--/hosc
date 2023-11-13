-- | Timeout, implemented independently of socket timeout setting.
module Sound.Osc.Time.Timeout where

import System.Timeout {- base -}

import Sound.Osc.Packet {- hsoc -}
import Sound.Osc.Transport.Fd {- hosc -}

-- | Variant of 'timeout' where time is given in fractional seconds.
timeout_r :: Double -> IO a -> IO (Maybe a)
timeout_r = timeout . floor . (* 1000000)

-- | Variant of 'recvPacket' that implements an /n/ second 'timeout'.
recvPacketTimeout :: Transport t => Double -> t -> IO (Maybe (Packet Message))
recvPacketTimeout n fd = timeout_r n (recvPacket fd)
