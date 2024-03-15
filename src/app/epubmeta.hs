import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)

import EpubTools.EpubMeta.Edit (edit)
import EpubTools.EpubMeta.Export (exportOpf)
import EpubTools.EpubMeta.Import (importOpf)
import EpubTools.EpubMeta.Opts (Mode (Edit, Export, Import, View),
  Options (..), parseOpts)
import EpubTools.EpubMeta.Util (EM, runEM)
import EpubTools.EpubMeta.View (view)


dispatch :: Options -> EM ()
dispatch (Options (View verbose) epubPath) = view verbose epubPath
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
