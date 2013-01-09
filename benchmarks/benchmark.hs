import Criterion.Main

import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl
import Sound.OpenSoundControl.NFData ()
import qualified Sound.OpenSoundControl.Coding.Decode.Binary as Binary
import qualified Sound.OpenSoundControl.Coding.Encode.Builder as Builder
import qualified Sound.OpenSoundControl.Coding.Decode.Base as Decode
import qualified Sound.OpenSoundControl.Coding.Encode.Base as Encode

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
