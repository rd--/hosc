-- | Data types for Osc messages, bundles and packets.
module Sound.Osc.Packet where

import Sound.Osc.Datum {- hosc -}

-- * Message

-- | Osc address pattern.  This is strictly an Ascii value, however it
--   is very common to pattern match on addresses and matching on
--   Data.ByteString.Char8 requires @OverloadedStrings@.
type Address_Pattern = String

-- | An Osc message, an 'Address_Pattern' and a sequence of 'Datum'.
data Message =
  Message
  {messageAddress :: !Address_Pattern
  ,messageDatum :: ![Datum]}
  deriving (Ord, Eq, Read, Show)

-- | 'Message' constructor.  It is an 'error' if the 'Address_Pattern'
-- doesn't conform to the Osc specification.
message :: Address_Pattern -> [Datum] -> Message
message a xs =
    case a of
      '/':_ -> Message a xs
      _ -> error "message: ill-formed address pattern"

messageSignature :: Message -> String
messageSignature = signatureFor . messageDatum

messageDescriptor :: Message -> Ascii
messageDescriptor = descriptor . messageDatum

-- * Bundle

{- | An Osc bundle, a 'Time' and a sequence of 'Message's.
Do not allow recursion, all contents must be messages.
-}
data Bundle t =
  Bundle
  {bundleTime :: !Time
  ,bundleMessages :: ![t]}
  deriving (Eq,Read,Show)

-- | Osc 'Bundle's can be ordered (time ascending).
instance Eq t => Ord (Bundle t) where
    compare (Bundle a _) (Bundle b _) = compare a b

-- | 'Bundle' constructor. It is an 'error' if the 'Message' list is empty.
bundle :: Time -> [t] -> Bundle t
bundle t xs =
    case xs of
      [] -> error "bundle: empty?"
      _ -> Bundle t xs

-- * Packet

-- | An Osc 'Packet' is either a 'Message' or a 'Bundle Message'.
data Packet t =
  Packet_Message {packetMessage :: !Message} |
  Packet_Bundle {packetBundle :: !(Bundle t)}
  deriving (Eq,Read,Show)

-- | 'Packet_Bundle' of 'bundle'.
p_bundle :: Time -> [t] -> Packet t
p_bundle t = Packet_Bundle . bundle t

-- | 'Packet_Message' of 'message'.
p_message :: Address_Pattern -> [Datum] -> Packet t
p_message a = Packet_Message . message a

{- | Constant indicating a bundle to be executed immediately.  It has the Ntp64 representation of @1@.

>>> immediately == (1 / (2 ^ 32))
True
-}
immediately :: Time
immediately = 1 / 2^(32::Int)

-- | The 'Time' of 'Packet', if the 'Packet' is a 'Message' this is 'immediately'.
packetTime :: Packet t -> Time
packetTime = at_packet (const immediately) bundleTime

-- | Retrieve the set of 'Message's from a 'Packet'.
packetMessages :: Packet Message -> [Message]
packetMessages = at_packet return bundleMessages

-- | If 'Packet' is a 'Message' add 'immediately' timestamp, else 'id'.
packet_to_bundle :: Packet Message -> Bundle Message
packet_to_bundle = at_packet (\m -> Bundle immediately [m]) id

-- | If 'Packet' is a 'Message' or a 'Bundle' with an /immediate/ time
-- tag and with one element, return the 'Message', else 'Nothing'.
packet_to_message :: Packet Message -> Maybe Message
packet_to_message p =
    case p of
      Packet_Bundle b ->
          case b of
            Bundle t [m] -> if t == immediately then Just m else Nothing
            _ -> Nothing
      Packet_Message m -> Just m

-- | Is 'Packet' immediate, ie. a 'Bundle' with timestamp 'immediately', or a plain Message.
packet_is_immediate :: Packet t -> Bool
packet_is_immediate = (== immediately) . packetTime

-- | Variant of 'either' for 'Packet'.
at_packet :: (Message -> a) -> (Bundle t -> a) -> Packet t -> a
at_packet f g p =
    case p of
      Packet_Message m -> f m
      Packet_Bundle b -> g b

-- * Address Query

-- | Does 'Message' have the specified 'Address_Pattern'.
message_has_address :: Address_Pattern -> Message -> Bool
message_has_address x = (== x) . messageAddress

-- | Do any of the 'Message's at 'Bundle Message' have the specified
-- 'Address_Pattern'.
bundle_has_address :: Address_Pattern -> Bundle Message -> Bool
bundle_has_address x = any (message_has_address x) . bundleMessages

-- | Does 'Packet' have the specified 'Address_Pattern', ie.
-- 'message_has_address' or 'bundle_has_address'.
packet_has_address :: Address_Pattern -> Packet Message -> Bool
packet_has_address x =
    at_packet (message_has_address x)
              (bundle_has_address x)
