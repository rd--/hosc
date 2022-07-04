import Criterion.Main {- criterion -}
import qualified Data.ByteString.Lazy as B {- bytestring -}

import Sound.Osc.Core {- hosc -}
import qualified Sound.Osc.Coding.Decode.Binary as Default
import qualified Sound.Osc.Coding.Encode.Builder as Default
import qualified Sound.Osc.Coding.Decode.Base as Base
import qualified Sound.Osc.Coding.Encode.Base as Base

import Sound.Osc.NFData ()

m0 :: Message
m0 = Message "/command" [Float 42.0,Int32 16,string "ASCII",Blob (B.pack [0..128])]

b0 :: Bundle
b0 = Bundle pi (replicate 12 m0)

p0 :: B.ByteString
p0 = Base.encodeBundle b0

g0 :: Benchmark
g0 = bgroup "encodeOSC"
     [bench "Base"  (nf Base.encodeBundle b0)
     ,bench "Builder" (nf Default.encodeBundle b0)]

g1 :: Benchmark
g1 = bgroup "decodeOSC"
     [bench "Base" (nf Base.decodePacket p0)
     ,bench "Binary" (nf Default.decodePacket p0)]

main :: IO ()
main = defaultMain [g0,g1]
