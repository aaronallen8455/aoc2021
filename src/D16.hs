module D16
  ( day16A
  , day16B
  ) where

import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import           Numeric

import           Util

data Packet =
  Pack
    { version :: Int
    , typ :: PacketTy
    }

data PacketTy
  = Literal Int
  | Op Int [Packet]

addVer :: Packet -> Int
addVer (Pack v ty) = (+ v) $
  case ty of
    Literal _ -> 0
    Op _ ps -> sum $ map addVer ps

eval :: Packet -> Int
eval (Pack _ ty) =
  case ty of
    Literal x -> x
    Op typ ps ->
      case typ of
        0 -> sum $ map eval ps
        1 -> product $  map eval ps
        2 -> minimum $ map eval ps
        3 -> maximum $ map eval ps
        5 -> if eval (head ps) > eval (ps !! 1)
                then 1
                else 0
        6 -> if eval (head ps) < eval (ps !! 1)
                then 1
                else 0
        7 -> if eval (head ps) == eval (ps !! 1)
                then 1
                else 0

day16A :: BS.ByteString -> BS.ByteString
day16A (BS.unpack . BS.init-> str) =
  let bin = concatMap (showBinary . fst . head . readHex . pure) str
      (Just p, _) = parsePacket bin
   in showBS $ addVer p

showBinary :: Int -> String
showBinary i = pad $ showBin i []
  where
    pad s = reverse . take 4 $ reverse s ++ repeat '0'

parsePacket :: String -> (Maybe Packet, String)
parsePacket str =
  let (ver, x) = splitAt 3 str
      verNum = fst . head $ readBin ver
      (typ, xx) = splitAt 3 x
      typNum = fst . head $ readBin typ
   in if typ == "100"
         then
          let (litStr, rest) = parseLiteral xx
              totalLen = length litStr + 6
           in (Just . Pack verNum . Literal . fst . head $ readBin litStr
              , rest)
         else case xx of
                '0' : xxx ->
                  let (len, xxxx) = splitAt 15 xxx
                      lenNum = fst . head $ readBin len
                      (packets, xxxxx) = splitAt lenNum xxxx
                      (parsed, _) = parsePackets packets
                   in (Just . Pack verNum $ Op typNum parsed
                      , xxxxx)
                '1' : xxx ->
                  let (numPacks, xxxx) = splitAt 11 xxx
                      npNum = fst . head $ readBin numPacks
                      (parsed, rest) = parseNumPackets npNum xxxx
                   in (Just . Pack verNum $ Op typNum parsed
                      , rest)
                _ -> (Nothing, str)

parsePackets :: String -> ([Packet], String)
parsePackets inp =
  case parsePacket inp of
    (Just p, rest) -> first (p:) $ parsePackets rest
    (Nothing, r) -> ([], r)

parseNumPackets :: Int -> String -> ([Packet], String)
parseNumPackets 0 inp = ([], inp)
parseNumPackets !n inp =
  case parsePacket inp of
    (Just p, rest) -> first (p:) $ parseNumPackets (n-1) rest
    (Nothing, r) -> ([], r)

parseLiteral :: String -> (String, String)
parseLiteral str =
  let (c:cnk, x) = splitAt 5 str
   in if c == '0'
         then (cnk, x)
         else first (cnk ++) $ parseLiteral x

day16B :: BS.ByteString -> BS.ByteString
day16B (BS.unpack . BS.init-> str) =
  let bin = concatMap (showBinary . fst . head . readHex . pure) str
      (Just p, _) = parsePacket bin
   in showBS $ eval p

