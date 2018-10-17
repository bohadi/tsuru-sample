module Main where

import qualified Options.Applicative as OA
import Options.Applicative ((<**>))
import Data.Semigroup      ((<>))

import Lib (unorderedPrint, reorderedPrint)

data Opts = Opts { file :: FilePath, willReorder :: Bool }

main :: IO ()
main = run =<< OA.execParser parser
  where
    opts   = Opts <$> OA.strArgument (OA.metavar "FILE" <>
                                      OA.help "Quote packets in pcap file")
                  <*> OA.switch      (OA.long "reorder" <> OA.short 'r' <>
                                      OA.help "Reorder on quote accept time")
    desc   = OA.briefDesc <> OA.progDesc "Parse and print quote packets in pcap FILE"
    parser = OA.info (opts <**> OA.helper) desc

run :: Opts -> IO ()
run (Opts f False) = unorderedPrint f
run (Opts f True ) = reorderedPrint f
