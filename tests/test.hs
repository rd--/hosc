{-# LANGUAGE ScopedTypeVariables #-}

import           Sound.OpenSoundControl (Packet)
import           Sound.OpenSoundControl.Arbitrary ()
import qualified Sound.OpenSoundControl.Coding.Decode.Base as DecodeBase
import qualified Sound.OpenSoundControl.Coding.Encode.Base as EncodeBase
import qualified Sound.OpenSoundControl.Coding.Decode.Binary as DecodeBinary
import qualified Sound.OpenSoundControl.Coding.Encode.Builder as EncodeBuilder
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
