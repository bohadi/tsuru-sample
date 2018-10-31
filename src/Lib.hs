{-# Language OverloadedStrings #-}

module Lib
  ( unorderedPrint
  , reorderedPrint
  , CacheType(..)
  ) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Zones       (utcToLocalTimeTZ)
import Data.Time.Zones.All   (tzByLabel, TZLabel(Asia__Tokyo))
import Data.Time.Format      (formatTime, defaultTimeLocale)

import qualified Network.Pcap          as P
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC (pack, putStr, readInt)

import qualified Data.List     as L (partition, sortOn)
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
type TimedQP = (Centiseconds, QuotePacket)

type ListCache   =      [TimedQP]
type SeqCache    = S.Seq TimedQP
type IterCache   =      [TimedQP]
type StreamCache =      [TimedQP]
data CacheType = SeqCache | ListCache | IterCache | StreamCache

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

formatPacketTime :: P.PktHdr -> B.ByteString
formatPacketTime hdr = BC.pack $ take 8 nearlyFormattedStr
--formatPacketTime hdr = BC.pack $ show $ P.hdrDiffTime hdr
  where
    nominalDiffTime    = (fromRational . toRational) $ P.hdrDiffTime hdr
    utcTime            = posixSecondsToUTCTime nominalDiffTime
    localTime          = utcToLocalTimeTZ (tzByLabel Asia__Tokyo) utcTime
    nearlyFormattedStr = formatTime defaultTimeLocale "%H%M%S%q" localTime

putPacket :: QuotePacket -> IO () -- TODO fold into single putStr call with big str
putPacket qp =
  let putOffer (Offer (q,p)) = BC.putStr $ B.concat [" ", q, "@", p]
  in do
    BC.putStr $ B.concat [packetTime qp, " ", acceptTime qp, " ", issueCode qp]
    mapM_ putOffer $ (reverse $ bids qp) <> asks qp
    putStrLn ""

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

reorderedPrint :: FilePath -> CacheType -> IO ()
reorderedPrint f c = do h <- P.openOffline f
                        case c of
                          SeqCache    -> void $ slooper h 0 S.Empty
                          ListCache   -> void $ llooper h 0 []
                          IterCache   -> void $ ilooper h 0 []  
                          StreamCache -> void $ mlooper h 0 [] -- TODO empty iter cache
  where
    -- list cache
    lsortAndPrint :: ListCache -> IO ()
    lsortAndPrint cache = mapM_ (putPacket . snd) $ L.sortOn fst $ reverse cache
    lreleasePending :: ListCache -> Centiseconds -> TimedQP -> IO ListCache
    lreleasePending cache threshtime tqp =
      let thresh item = fst item < (threshtime - maxDelayCS)
          (pending, cache') = L.partition thresh cache
      in lsortAndPrint pending >> return (tqp : cache')
    llooper :: P.PcapHandle -> Centiseconds -> ListCache -> IO ()
    llooper h prevRelease cache = do
      (hdr, pl) <- P.nextBS h
      let qpl = qPayload pl
      case (pl,qpl) of
            ("",_) -> lsortAndPrint cache          -- EOF
            (_,"") -> llooper h prevRelease cache  -- no quote in packet
            _ -> let qp = parseQuote hdr qpl
                     qt = toCentiseconds $ acceptTime qp
                     pt = toCentiseconds $ packetTime qp
                 in if prevRelease < pt - maxDelayCS
                      then lreleasePending cache pt (qt,qp) >>= llooper h pt
                      else llooper h prevRelease $ (qt,qp) : cache
    -- seq cache
    ssortAndPrint :: SeqCache -> IO ()
    ssortAndPrint cache = mapM_ (putPacket . snd) $ S.sortOn fst cache
    sreleasePending :: SeqCache -> Centiseconds -> TimedQP -> IO SeqCache
    sreleasePending cache threshtime tqp =
      let thresh item = fst item < (threshtime - maxDelayCS)
          (pending, cache') = S.partition thresh cache
      in ssortAndPrint pending >> return (cache' |> tqp)
    slooper :: P.PcapHandle -> Centiseconds -> SeqCache -> IO ()
    slooper h prevRelease cache = do
      (hdr, pl) <- P.nextBS h
      let qpl = qPayload pl
      case (pl,qpl) of
            ("",_) -> ssortAndPrint cache          -- EOF
            (_,"") -> slooper h prevRelease cache  -- no quote in packet
            _ -> let qp = parseQuote hdr qpl
                     qt = toCentiseconds $ acceptTime qp
                     pt = toCentiseconds $ packetTime qp
                 in if prevRelease < pt - maxDelayCS
                      then sreleasePending cache pt (qt,qp) >>= slooper h pt
                      else slooper h prevRelease $ cache |> (qt,qp)
    -- iter cache
    isortAndPrint :: IterCache -> IO ()
    isortAndPrint cache = mapM_ (putPacket . snd) $ L.sortOn fst cache
    ireleasePending :: IterCache -> Centiseconds -> TimedQP -> IO IterCache
    ireleasePending cache threshtime tqp = undefined
    ilooper :: P.PcapHandle -> Centiseconds -> IterCache -> IO ()
    ilooper h prevRelease cache = undefined
    -- stream cache
    msortAndPrint :: StreamCache -> IO ()
    msortAndPrint cache = mapM_ (putPacket . snd) $ L.sortOn fst cache
    mreleasePending :: StreamCache -> Centiseconds -> TimedQP -> IO StreamCache
    mreleasePending cache threshtime tqp = undefined
    mlooper :: P.PcapHandle -> Centiseconds -> StreamCache -> IO ()
    mlooper h prevRelease cache = undefined
