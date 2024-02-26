import Data.Maybe
import System.Environment ( getArgs )
import System.Exit

import EpubTools.EpubMeta.Display
import EpubTools.EpubMeta.Edit
import EpubTools.EpubMeta.Export
import EpubTools.EpubMeta.Import
import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


dispatch :: Options -> [FilePath] -> EM ()
dispatch opts (f:[]) | optEdit opts /= NotEditing = edit opts f
dispatch opts (f:[]) | isJust . optImport $ opts  = importOpf opts f
dispatch opts (f:[]) | optExport opts /= NoExport = exportOpf opts f
dispatch opts (f:[])                              = display opts f
dispatch _    _                                   = throwError usageText


main :: IO ()
main = do
   result <- runEM $
      liftIO getArgs >>= parseOpts >>= uncurry dispatch
   either exitFail (const (exitWith ExitSuccess)) result

   where
      exitFail msg = do
         putStrLn msg
         exitWith $ ExitFailure 1
