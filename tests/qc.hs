{-# LANGUAGE ScopedTypeVariables #-}

import Test.Framework {- test-framework -}
import Test.Framework.Providers.QuickCheck2 {- test-framework-quickcheck2 -}

import qualified Sound.OSC.Coding.Decode.Base as Base
import qualified Sound.OSC.Coding.Encode.Base as Base
import qualified Sound.OSC.Coding.Decode.Binary as Default
import qualified Sound.OSC.Coding.Encode.Builder as Default

import Sound.OSC.Arbitrary ()

are_eq :: Eq a => (t -> a) -> (t -> a) -> t -> Bool
are_eq p q x = p x == q x

is_id :: Eq a => (a -> a) -> a -> Bool
is_id f x = f x == x

tests :: [Test]
tests =
  [testProperty "encode/eq" (are_eq Default.encodePacket Base.encodePacket)
  ,testProperty "encode/decode (Base)" (is_id (Base.decodePacket . Base.encodePacket))
  ,testProperty "encode/decode (Binary)" (is_id (Default.decodePacket . Base.encodePacket))]

main :: IO ()
main = defaultMain tests
