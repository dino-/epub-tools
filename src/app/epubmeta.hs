import System.Exit

import EpubTools.EpubMeta.Display
import EpubTools.EpubMeta.Edit
import EpubTools.EpubMeta.Export
import EpubTools.EpubMeta.Import
import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


dispatch :: Options -> EM ()
dispatch (Options (View verbose) epubPath) = display verbose epubPath
dispatch (Options (Export output) epubPath) = exportOpf output epubPath
dispatch (Options (Edit backup) epubPath) = edit backup epubPath
dispatch (Options (Import importPath backup) epubPath) = importOpf importPath backup epubPath


main :: IO ()
main = do
  opts <- parseOpts
  result <- runEM $ dispatch opts
  either exitFail (const (exitWith ExitSuccess)) result

  where
    exitFail msg = do
      putStrLn msg
      exitWith $ ExitFailure 1
