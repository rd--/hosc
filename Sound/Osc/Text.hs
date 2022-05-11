-- | A simple and unambigous text encoding for Osc.
module Sound.Osc.Text where

import Control.Monad {- base -}
import Data.Char {- base -}
import Numeric {- base -}
import Text.Printf {- base -}

import qualified Text.ParserCombinators.Parsec as P {- parsec -}

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet  {- hosc3 -}
import Sound.Osc.Time  {- hosc3 -}

-- | Precision value for floating point numbers.
type FpPrecision = Maybe Int

{- | Variant of 'showFFloat' that deletes trailing zeros.

> map (showFloatWithPrecision (Just 4)) [1, 2.0, pi] == ["1.0", "2.0", "3.1416"]
-}
showFloatWithPrecision :: RealFloat n => FpPrecision -> n -> String
showFloatWithPrecision p n =
    let s = showFFloat p n ""
        s' = dropWhile (== '0') (reverse s)
    in case s' of
         '.':_ -> reverse ('0' : s')
         _ -> reverse s'

{- | Hex encoded byte sequence.

> showBytes [0, 15, 16, 144, 255] == "000f1090ff"
-}
showBytes :: [Int] -> String
showBytes = concatMap (printf "%02x")

{- | Escape whites space (space, tab, newline) and the escape character (backslash).

> mapM_ (putStrLn .  escapeString) ["str", "str ", "st r", "s\tr", "s\\tr", "\nstr"]
-}
escapeString :: String -> String
escapeString txt =
  case txt of
    [] -> []
    c:txt' -> if c `elem` "\\\t\n " then '\\'  : c : escapeString txt' else c : escapeString txt'

{- | Printer for Datum.

> aDatumSeq = [Int32 1,Float 1.2,string "str",midi (0,0x90,0x40,0x60),blob [12,16], TimeStamp 100.0]
> map (showDatum (Just 5)) aDatumSeq == ["1","1.2","str","00904060","0c10","429496729600"]
-}
showDatum :: FpPrecision -> Datum -> String
showDatum p d =
    case d of
      Int32 n -> show n
      Int64 n -> show n
      Float n -> showFloatWithPrecision p n
      Double n -> showFloatWithPrecision p n
      AsciiString s -> escapeString (ascii_to_string s)
      Blob s -> showBytes (blob_unpack_int s)
      TimeStamp t -> show (ntpr_to_ntpi t)
      Midi m -> showBytes (midi_unpack_int m)

{- | Printer for Message.

> aMessage = Message "/addr" [Int32 1, Int64 2, Float 3, Double 4, string "five", blob [6, 7], midi (8, 9, 10, 11)]
> showMessage (Just 4) aMessage

> aMessageSeq = [Message "/c_set" [Int32 1, Float 2.3], Message "/s_new" [string "sine", Int32 (-1), Int32 1, Int32 1]]
> map (showMessage (Just 4)) aMessageSeq
-}
showMessage :: FpPrecision -> Message -> String
showMessage precision aMessage =
  unwords
  [messageAddress aMessage
  ,messageSignature aMessage
  ,unwords (map (showDatum precision) (messageDatum aMessage))]

{- | Printer for Bundle

> aBundle = Bundle 1 [Message "/c_set" [Int32 1, Float 2.3, Int64 4, Double 5.6], Message "/memset" [string "addr", blob [7, 8]]]
> showBundle (Just 4) aBundle
-}
showBundle :: FpPrecision -> Bundle -> String
showBundle precision aBundle =
  let messages = bundleMessages aBundle
  in unwords
     ["#bundle"
     ,show (ntpr_to_ntpi (bundleTime aBundle))
     ,show (length messages)
     ,unwords (map (showMessage precision) messages)]

-- | Printer for Packet.
showPacket :: FpPrecision -> Packet -> String
showPacket precision = at_packet (showMessage precision) (showBundle precision)

-- * Parser

-- | A character parser with no user state.
type P a = P.GenParser Char () a

-- | Run p then q, returning result of p.
(>>~) :: Monad m => m t -> m u -> m t
p >>~ q = p >>= \x -> q >> return x

-- | /p/ as lexeme, i.e. consuming any trailing white space.
lexemeP :: P t -> P t
lexemeP p = p >>~ P.many P.space

-- | Any non-space character.  Allow escaped space.
stringCharP :: P Char
stringCharP = (P.char '\\' >> P.space) P.<|> P.satisfy (\c -> not (isSpace c))

-- | Parser for string.
stringP :: P String
stringP = lexemeP (P.many1 stringCharP)

-- | Parser for Osc address.
oscAddressP :: P String
oscAddressP = do
  forwardSlash <- P.char '/'
  address <- stringP
  return (forwardSlash : address)

-- | Parser for Osc signature.
oscSignatureP :: P String
oscSignatureP = lexemeP (do
  comma <- P.char ','
  types <- P.many1 (P.oneOf "ifsbhtdm") -- 1.0 = ifsb 2.0 = htdm
  return (comma : types))

-- | Parser for decimal digit.
digitP :: P Char
digitP = P.oneOf "0123456789"

allowNegativeP :: Num n => P n -> P n
allowNegativeP p = do
  let optionMaybe x = P.option Nothing (liftM Just x) -- hugs...
  maybeNegative <- optionMaybe (P.char '-')
  number <- p
  return (maybe number (const (negate number)) maybeNegative)

-- | Parser for non-negative integer.
nonNegativeIntegerP :: (Integral n, Read n) => P n
nonNegativeIntegerP = lexemeP (fmap read (P.many1 digitP))

-- | Parser for integer.
integerP :: (Integral n, Read n) => P n
integerP = allowNegativeP nonNegativeIntegerP

-- | Parser for non-negative float.
nonNegativeFloatP :: (Fractional n, Read n) => P n
nonNegativeFloatP = lexemeP (do
  integerPart <- P.many1 digitP
  _ <- P.char '.'
  fractionalPart <- P.many1 digitP
  return (read (concat [integerPart, ".", fractionalPart])))

-- | Parser for non-negative float.
floatP :: (Fractional n, Read n) => P n
floatP = allowNegativeP nonNegativeFloatP

-- | Parser for hexadecimal digit.
hexdigitP :: P Char
hexdigitP = P.oneOf "0123456789abcdef"

-- | Byte parser.
byteP :: (Integral n, Read n) => P n
byteP = do
  c1 <- hexdigitP
  c2 <- hexdigitP
  case readHex [c1, c2] of
    [(r,"")] -> return r
    _ -> error "byteP?"

-- | Byte sequence parser.
byteSeqP :: (Integral n, Read n) => P [n]
byteSeqP = lexemeP (P.many1 byteP)

-- | Datum parser.
datumP :: Char -> P Datum
datumP typeChar = do
  case typeChar of
    'i' -> fmap Int32 integerP
    'f' -> fmap Float floatP
    's' -> fmap string stringP
    'b' -> fmap blob byteSeqP
    'h' -> fmap Int64 integerP
    'd' -> fmap Double floatP
    'm' -> fmap (Midi . midi_pack) (replicateM 4 byteP)
    't' -> fmap (TimeStamp . ntpi_to_ntpr) integerP
    _ -> error "datumP: type?"

-- | Message parser.
messageP :: P Message
messageP = do
  address <- oscAddressP
  typeSignature <- oscSignatureP
  datum <- mapM datumP (tail typeSignature)
  return (Message address datum)

-- | Bundle tag parser.
bundleTagP :: P String
bundleTagP = lexemeP (P.string "#bundle")

-- | Bundle parser.
bundleP :: P Bundle
bundleP = do
  _ <- bundleTagP
  timestamp <- fmap ntpi_to_ntpr integerP
  messageCount <- integerP
  messages <- replicateM messageCount messageP
  return (Bundle timestamp messages)

-- | Packet parser.
packetP :: P Packet
packetP = (fmap Packet_Bundle bundleP) P.<|> (fmap Packet_Message messageP)

-- | Run parser.
runP :: P t -> String -> t
runP p txt =
  case P.parse p "" txt of
    Left err -> error (show err)
    Right r -> r

{- | Run datum parser.

> parseDatum 'i' "-1" == Int32 (-1)
> parseDatum 'f' "-2.3" == Float (-2.3)
-}
parseDatum :: Char -> String -> Datum
parseDatum typ = runP (datumP typ)

{- | Run message parser.

> aMessageSeq = [Message "/c_set" [Int32 1, Float 2.3, Int64 4, Double 5.6], Message "/memset" [string "addr", blob [7, 8]]]
> map (parseMessage . showMessage (Just 4)) aMessageSeq  == aMessageSeq
-}
parseMessage :: String -> Message
parseMessage = runP messageP

{- | Run bundle parser.

> aBundle = Bundle 1 [Message "/c_set" [Int32 1, Float 2.3, Int64 4, Double 5.6], Message "/memset" [string "addr", blob [7, 8]]]
> parseBundle (showBundle (Just 4) aBundle) == aBundle
-}
parseBundle :: String -> Bundle
parseBundle = runP bundleP

{- | Run packet parser.

> aPacket = Packet_Bundle (Bundle 1 [Message "/c_set" [Int32 1, Float 2.3, Int64 4, Double 5.6], Message "/memset" [string "addr", blob [7, 8]]])
> parsePacket (showPacket (Just 4) aPacket) == aPacket
-}
parsePacket :: String -> Packet
parsePacket = runP packetP
