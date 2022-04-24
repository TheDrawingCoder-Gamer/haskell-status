module Status.Plugins.AudioInfo where 

import System.Process
import Data.List.Extra (trim) 
import Status.Config
import Text.Format ((~~), (~%))
import Data.String
import Data.Text qualified as T
import Control.Lens.Tuple
import Control.Lens (set, view)
import System.Exit
data AudioInfo = AudioInfo 
    { audioVolume :: Int
    , audioMute   :: Bool } 

getAudioInfo :: IO AudioInfo 
getAudioInfo = do 
    vol <- read @Int . trim <$> readProcess "pamixer" ["--get-volume"] []
    muted <-  (==ExitSuccess) . view _1 <$> readProcessWithExitCode "pamixer" ["--get-mute"] []
    pure $ AudioInfo vol muted 

parseExoticBool "true" = True 
parseExoticBool "false" = False 
parseExoticBool _       = error "parseExoticBool: no parse"

showMuted :: Bool -> String
showMuted True = "muted" 
showMuted False = "unmuted"
getAudioStr :: Settings -> IO String 
getAudioStr Settings{settingsAudio=AudioSettings{audioFormat, audioFormatMuted}} = do
    info <- getAudioInfo
    let volStr = show (audioVolume info) ++ "%"
    pure $  if audioMute info then 
                fromString (T.unpack audioFormatMuted)
                    ~~ ("volume" ~% volStr) 
                    ~~ ("muted"  ~% showMuted True) 
            else 
                fromString (T.unpack audioFormat) 
                    ~~ ("volume" ~% volStr) 
                    ~~ ("muted"  ~% showMuted False)
    
