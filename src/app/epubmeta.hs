import System.Exit

import EpubTools.EpubMeta.Display
import EpubTools.EpubMeta.Edit
import EpubTools.EpubMeta.Export
import EpubTools.EpubMeta.Import
import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


dispatch :: Options -> EM ()
dispatch (Options (View verbose) f) = display verbose f
dispatch (Options (Export output) f) = exportOpf output f
dispatch (Options (Edit backup) f) = edit backup f
dispatch (Options (Import importPath backup) f) = importOpf importPath backup f


main :: IO ()
main = do
  opts <- parseOpts
  -- print opts
  result <- runEM $ dispatch opts
  either exitFail (const (exitWith ExitSuccess)) result

  where
    exitFail msg = do
      putStrLn msg
      exitWith $ ExitFailure 1
