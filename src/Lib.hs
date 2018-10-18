{-# Language OverloadedStrings #-}

module Lib
  ( unorderedPrint
  , reorderedPrint
  ) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Zones       (utcToLocalTimeTZ)
import Data.Time.Zones.All   (tzByLabel, TZLabel(Asia__Tokyo))
import Data.Time.Format      (formatTime, defaultTimeLocale)

import qualified Network.Pcap          as P
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC (pack, putStr, readInt)

import qualified Data.Sequence as S (Seq(Empty), partition, sortOn)
import           Data.Sequence ((|>))

import Data.Function ((&))
import Data.Functor  ((<&>))
import Data.Tuple    (swap)
import Data.Maybe    (fromJust)
import Control.Monad (unless, void)

newtype Offer = Offer (Quantity, Price)
type Quantity = B.ByteString
type Price = B.ByteString

type HHMMSSss = B.ByteString
type Centiseconds = Int
maxDelayCS :: Centiseconds
maxDelayCS = 3 * 100

data QuotePacket =
  QuotePacket {
      packetTime :: HHMMSSss
    , acceptTime :: HHMMSSss
    , issueCode  :: B.ByteString
    , bids :: [Offer]
    , asks :: [Offer]
  }

dropTake :: B.ByteString -> Int -> Int -> B.ByteString
dropTake bs idx len = bs & B.drop idx & B.take len

toCentiseconds :: HHMMSSss -> Centiseconds
toCentiseconds t = sum $ zipWith (*) [360000, 6000, 100, 1] (hmsc <$> [0..3])
  where hmsc i = fst $ fromJust $ BC.readInt $ dropTake t (2*i) 2

parseQuote :: P.PktHdr -> B.ByteString -> QuotePacket
parseQuote hdr qpl =
  let toOffers bs =
        [0..4] <&> (\ith -> Offer $ swap $ B.splitAt 5 $ dropTake bs (ith*12) 12)
  in QuotePacket {
         packetTime = formatPacketTime hdr
       , acceptTime =            dropTake qpl 206  8
       , issueCode  =            dropTake qpl   5 12
       , bids       = toOffers $ dropTake qpl  29 60
       , asks       = toOffers $ dropTake qpl  96 60
     } -- see packet specification
    
formatPacketTime :: P.PktHdr -> B.ByteString
formatPacketTime hdr = BC.pack $ take 8 nearlyFormattedStr
  where
    nominalDiffTime    = (fromRational . toRational) $ P.hdrDiffTime hdr
    utcTime            = posixSecondsToUTCTime nominalDiffTime
    localTime          = utcToLocalTimeTZ (tzByLabel Asia__Tokyo) utcTime
    nearlyFormattedStr = formatTime defaultTimeLocale "%H%M%S%q" localTime

putPacket :: QuotePacket -> IO ()
putPacket qp =
  let putOffer (Offer (q,p)) = BC.putStr $ B.concat [" ", q, "@", p]
  in do
    BC.putStr $ B.concat [packetTime qp, " ", acceptTime qp, " ", issueCode qp]
    mapM_ putOffer $ (reverse $ bids qp) <> asks qp
    putStrLn ""

qPayload :: B.ByteString -> B.ByteString
qPayload pl = snd $ B.breakSubstring "B6034" pl

unorderedPrint :: FilePath -> IO ()
unorderedPrint f = do h <- P.openOffline f
                      void $ P.dispatchBS h (-1) parseAndPut
  where
    parseAndPut :: P.PktHdr -> B.ByteString -> IO ()
    parseAndPut hdr pl =
      let qpl = qPayload pl
      in unless (B.null qpl) $ putPacket $ parseQuote hdr qpl

type Cache = S.Seq (Centiseconds, QuotePacket) -- quote accept time

reorderedPrint :: FilePath -> IO ()
reorderedPrint f = P.openOffline f >>= viaSeq S.Empty
  where
    sortAndPrint :: Cache -> IO ()
    sortAndPrint cache = mapM_ (putPacket . snd) $ S.sortOn fst cache

    viaSeq :: Cache -> P.PcapHandle -> IO ()
    viaSeq cache h = do
      (hdr, pl) <- P.nextBS h
      let qpl = qPayload pl
      case (pl,qpl) of
            ("",_) -> sortAndPrint cache -- EOF
            (_,"") -> viaSeq cache h    -- no quote in packet
            _ -> sortAndPrint pending >> viaSeq (cache' |> (qt,qp)) h
                    where qp = parseQuote hdr qpl
                          qt = toCentiseconds $ acceptTime qp
                          pt = toCentiseconds $ packetTime qp
                          thresh c = fst c < (pt - maxDelayCS)
                          (pending, cache') = S.partition thresh cache
