module EpubTools.EpubName.Prompt
   ( PromptResult (..)
   , prompt
   , continue
   )
   where

import Data.Char ( toLower )


data PromptResult
   = Yes
   | No
   | Quit
   deriving Eq


prompt :: IO PromptResult
prompt = do
   putStr "Link file to this new location? [Ynq], or ? for more options: "
   c <- getChar
   putStr "\n"

   case (toLower c) of
      'y'  -> return Yes
      ' '  -> return Yes
      '\n' -> return Yes
      'n'  -> return No
      'q'  -> return Quit
      '?'  -> more >> prompt
      _    -> putStrLn "Unknown choice" >> prompt


continue :: PromptResult -> Bool
continue Quit = False
continue _    = True


more :: IO ()
more = putStr . unlines $
   [ "y: rename this file"
   , "n: don't rename this file"
   , "q: cancel now, don't rename any files beyond this point"
   , "<space> or <enter>: accept the default (which is capitalized)"
   ]
