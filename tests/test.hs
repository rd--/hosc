{-# LANGUAGE ScopedTypeVariables #-}

import           Sound.OSC.Arbitrary ()
import qualified Sound.OSC.Coding.Decode.Base as DecodeBase
import qualified Sound.OSC.Coding.Encode.Base as EncodeBase
import qualified Sound.OSC.Coding.Decode.Binary as DecodeBinary
import qualified Sound.OSC.Coding.Encode.Builder as EncodeBuilder
import           Sound.OSC.Type (Packet)
import           Test.Framework (Test, defaultMain)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests =
    [ testProperty "encodePacket (Builder)" $ \(osc :: Packet) ->
        EncodeBuilder.encodePacket osc == EncodeBase.encodePacket osc
    , testProperty "encodePacket/decodePacket" $ \(osc :: Packet) ->
        DecodeBase.decodePacket (EncodeBase.encodePacket osc) == osc
    , testProperty "decodePacket (Get)" $ \(osc :: Packet) ->
        DecodeBinary.decodePacket (EncodeBase.encodePacket osc) == osc
    ]

main :: IO ()
main = defaultMain tests
