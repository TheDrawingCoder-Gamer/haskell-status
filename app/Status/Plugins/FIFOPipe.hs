module Status.Plugins.FIFOPipe where 

import System.IO
import Status.Config 
import Control.Concurrent
import Control.Concurrent.MVar
import Data.HashMap.Strict qualified as HM 
import Data.Traversable
import Status.Formatter
import Control.Exception (bracket, tryJust)
import Control.Concurrent.Async qualified as Async
import Data.Stringly
import Data.String
import Text.Format qualified as F
import System.IO.Error (isEOFError)
import Control.Monad (guard)
import Data.Foldable
import System.Posix.IO
import System.Posix.Types (Fd)
import GHC.IO.Handle.FD (openFileBlocking)
pollFIFO :: Settings -> MVar SystemInfo -> Handle -> (Settings -> SystemInfo -> String -> IO SystemInfo ) -> IO () 
pollFIFO config msys handle cb = do
    input <- hGetLine handle
    sysinfo <- takeMVar msys
    sysinfo' <-  cb config sysinfo input 
    putMVar msys sysinfo'
    
setupFIFOs :: Settings -> MVar SystemInfo -> IO () 
setupFIFOs conf@Settings{settingsFifo} mvar = do  
    let 
        daMap = HM.fromList $ map (\v -> (toString $ fifoName v, v)) settingsFifo
    handles <- traverse (\v -> openFileBlocking (toString $ fifoPath v) ReadMode) daMap
    Async.mapConcurrently_ (\v ->  pollFIFO conf mvar (handles HM.! toString (fifoName v)) 
                                    (\c s i -> do  
                                        let finput = fromString (toString $ fifoFormat v) F.~~ F.formattable i [] :: String
                                        let sysinfo = s { sysinfFifo = HM.insert (toString $ fifoName v) i (sysinfFifo s) } 
                                        putStrLn $ formatGeneral (fromString $ toString $ settingsFormat c) sysinfo 
                                        pure sysinfo)) daMap
