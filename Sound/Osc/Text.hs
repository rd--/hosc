-- | A simple and unambigous text encoding for Osc.
module Sound.Osc.Text where

import Control.Monad {- base -}
import Data.Char {- base -}
import Numeric {- base -}
import Text.Printf {- base -}

import qualified Text.ParserCombinators.Parsec as P {- parsec -}
--import qualified Text.Parsec.Language as P {- parsec -}
--import qualified Text.Parsec.String as P {- parsec -}
--import qualified Text.Parsec.Token as P {- parsec -}

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

-- | Hex encoded byte sequence.
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

{- | Printer for 'Datum'.

> let d = [Int32 1,Float 1.2,string "str",midi (0,0x90,0x40,0x60),blob [12,16], TimeStamp 100.0]
> map (showDatum (Just 5)) d == ["1","1.2","str","00904060","0c10","429496729600"]
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

> let format = showMessage (Just 4)
> format (Message "/c_set" [Int32 1, Float 2.3])
> format (Message "/s_new" [string "sine", Int32 (-1), Int32 1, Int32 1])
> format (Message "/addr" [Int32 1, Int64 2, Float 3, Double 4, string "five", blob [6, 7], midi (8, 9, 10, 11)])
-}
showMessage :: FpPrecision -> Message -> String
showMessage precision aMessage =
  unwords
  [messageAddress aMessage
  ,messageSignature aMessage
  ,unwords (map (showDatum precision) (messageDatum aMessage))]

{- | Printer for Bundle

> let format = showBundle (Just 4)
> format (Bundle 1 [Message "/c_set" [Int32 1, Float 2.3, Int64 4, Double 5.6], Message "/memset" [string "addr", blob [7, 8]]])
-}
showBundle :: FpPrecision -> Bundle -> String
showBundle precision aBundle =
  unwords
  ["#bundle"
  ,show (ntpr_to_ntpi (bundleTime aBundle))
  ,unwords (map (showMessage precision) (bundleMessages aBundle))]

-- | Printer for Packet.
showPacket :: FpPrecision -> Packet -> String
showPacket precision = at_packet (showMessage precision) (showBundle precision)

-- * Parser

-- | A 'Char' parser with no user state.
type P a = P.GenParser Char () a

-- | Run p then q, returning result of p.
(>>~) :: Monad m => m t -> m u -> m t
p >>~ q = p >>= \x -> q >> return x

-- | /p/ consuming any trailing separators.
lexemeP :: P t -> P t
lexemeP p = p >>~ P.many P.space

-- | Any non-space and non-; character.  Allow escaped space.
stringCharP :: P Char
stringCharP = (P.char '\\' >> P.space) P.<|> P.satisfy (\c -> not (isSpace c) && c /= ';')

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

-- | Parser for semicolon lexeme.
semicolonP :: P Char
semicolonP = lexemeP (P.char ';')

-- | Parser for decimal digit.
digitP :: P Char
digitP = P.oneOf "0123456789"

-- | Parser for integer.
integerP :: (Integral n, Read n) => P n
integerP = lexemeP (fmap read (P.many1 digitP))

-- | Parser for float.
floatP :: (Fractional n, Read n) => P n
floatP = lexemeP (do
  integerPart <- P.many1 digitP
  _ <- P.char '.'
  fractionalPart <- P.many1 digitP
  return (read (concat [integerPart, ".", fractionalPart])))

-- | Parser for hexadecimal digit.
hexdigitP :: P Char
hexdigitP = P.oneOf "0123456789abcdef"

byteP :: (Integral n, Read n) => P n
byteP = do
  c1 <- hexdigitP
  c2 <- hexdigitP
  case readHex [c1, c2] of
    [(r,"")] -> return r
    _ -> error "byteP?"

byteSeqP :: (Integral n, Read n) => P [n]
byteSeqP = lexemeP (P.many1 byteP)

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

messageP :: P Message
messageP = do
  address <- oscAddressP
  typeSignature <- oscSignatureP
  datum <- mapM datumP (tail typeSignature)
  return (Message address datum)

runP :: P t -> String -> t
runP p txt =
  case P.parse p "" txt of
    Left err -> error (show err)
    Right r -> r

{- | runP of messageP

> map parseMessage ["/c_set ,ifhd 1 2.3 4 5.6", "/memset ,sb addr 0708"]
-}
parseMessage :: String -> Message
parseMessage = runP messageP
