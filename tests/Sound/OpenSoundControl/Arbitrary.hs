module Sound.OpenSoundControl.Arbitrary () where

import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl
import Test.QuickCheck

genTime :: Gen Time
-- Avoid floating point representation/conversion errors
genTime = ntpi_to_ntpr <$> arbitrary

genString :: Gen String
genString = resize 128 (listOf (arbitrary `suchThat` (/= '\0')))

instance Arbitrary Datum where
    arbitrary = oneof [
        Int       <$> arbitrary
      , Float     <$> realToFrac <$> (arbitrary :: Gen Float)
      , Double    <$> arbitrary
      , String    <$> genString
      , Blob      <$> B.pack <$> resize 128 arbitrary
      , TimeStamp <$> genTime
      , Midi      <$> arbitrary
      ]

genMessage :: Gen Message
genMessage = message <$> ("/"++) <$> genString <*> resize 32 (listOf1 arbitrary)

instance Arbitrary Packet where
    arbitrary = oneof [
        Packet_Message <$> genMessage
      , p_bundle <$> genTime <*> resize 32 (listOf1 genMessage)
      ]
