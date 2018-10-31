module Main where

import qualified Options.Applicative as OA
import Options.Applicative ((<**>))
import Data.Semigroup      ((<>))

import Lib (unorderedPrint, reorderedPrint, CacheType(..))

data Opts = Opts { file :: FilePath, cacheType :: String }

reorderHelpStr :: String
reorderHelpStr =
  "Reorder on quote accept time, using cache type: list, seq, iteratee, streamly, (else unordered)"

main :: IO ()
main = run =<< OA.execParser parser
  where
    opts   = Opts <$> OA.strArgument (OA.metavar "FILE" <>
                                      OA.help "Quote packets in pcap file")
                  <*> OA.strOption   (OA.long "reorder" <> OA.short 'r' <>
                                      OA.help reorderHelpStr)
    desc   = OA.briefDesc <> OA.progDesc "Parse and print quote packets in pcap FILE"
    parser = OA.info (opts <**> OA.helper) desc

run :: Opts -> IO ()
run (Opts f "list")     = putStrLn "Reordering (list)"   >> reorderedPrint f ListCache
run (Opts f "seq")      = putStrLn "Reordering (seq)"    >> reorderedPrint f SeqCache
run (Opts f "iteratee") = putStrLn "Reordering (iter)"   >> reorderedPrint f IterCache
run (Opts f "streamly") = putStrLn "Reordering (stream)" >> reorderedPrint f StreamCache
run (Opts f _)          = putStrLn "Unordered" >> unorderedPrint f
