import Control.Monad {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Word {- base -}
import System.Environment {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Network.Socket.ByteString as N {- network -}

import qualified Sound.OSC.FD as O {- hosc -}

udp_recv_bytes :: O.UDP -> IO B.ByteString
udp_recv_bytes = flip N.recv 8192 . O.udpSocket

u8_to_char :: Word8 -> Char
u8_to_char = toEnum . fromIntegral

print_u8 :: Word8 -> String
print_u8 x = printf "%02x (%c)" x (u8_to_char x)

bytes_pp :: B.ByteString -> [String]
bytes_pp =
  let f = intercalate "  " . map print_u8
  in map f . chunksOf 4 . B.unpack

osc_trace :: Int -> IO ()
osc_trace p = do
  let pr b = putStrLn (unlines (bytes_pp b)) >>
             putStrLn (O.packetPP (Just 5) (O.decodePacket_strict b) ++ "\n")
      fn fd = forever (udp_recv_bytes fd >>= pr)
  O.withTransport (O.udp_server p) fn

main :: IO ()
main = do
  a <- getArgs
  case a of
    [n] -> osc_trace (read n)
    _ -> putStrLn "trace port:int"
