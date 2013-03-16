import Criterion.Main {- criterion -}
import qualified Data.ByteString.Lazy as B {- bytestring -}

import Sound.OSC
import qualified Sound.OSC.Coding.Decode.Binary as Binary
import qualified Sound.OSC.Coding.Encode.Builder as Builder
import qualified Sound.OSC.Coding.Decode.Base as Decode
import qualified Sound.OSC.Coding.Encode.Base as Encode
import Sound.OSC.NFData ()

type EncodingFunc = Bundle -> B.ByteString
type DecodingFunc = B.ByteString -> Packet

main :: IO ()
main =
    defaultMain [
        bgroup "encodeOSC" [
            bench "Encode"  (nf (Encode.encodeBundle :: EncodingFunc) b)
          , bench "Builder" (nf (Builder.encodeBundle :: EncodingFunc) b)
          ]
      , bgroup "decodeOSC" [
            bench "Decode" (nf (Decode.decodePacket :: DecodingFunc) p)
          , bench "Binary" (nf (Binary.decodePacket :: DecodingFunc) p)
          ]
      ]
    where
        m = Message "/fooblah" [Float 42
                               ,Int 16
                               ,String "yeah"
                               ,Blob (B.pack [0..128])]
        b = Bundle pi (replicate 12 m)
        p = Encode.encodeBundle b
