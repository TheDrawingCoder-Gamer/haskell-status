module Status.Plugins.DBusInfo where

import DBus.Client
import Control.Concurrent.MVar
import Status.Config qualified as C
import Data.HashMap.Strict qualified as HM
import Status.Formatter
import Data.Stringly
import Data.String
import Data.Foldable (traverse_)
import Data.List.Extra (replace)
import Text.Format ((~~))
import System.IO.Unsafe (unsafePerformIO)
import DBus.Internal.Message (Signal(..))
import DBus.Internal.Types (showValue, varToVal, )
import Data.Text qualified as T
{-# NOINLINE signalClient #-}
signalClient = unsafePerformIO connectSession
{-# NOINLINE sysinfoMvar #-}
sysinfoMvar = unsafePerformIO (newMVar (C.SystemInfo Nothing Nothing Nothing Nothing Nothing Nothing HM.empty))
setupClient :: C.Settings -> C.DBusSettings -> IO ()
setupClient settings C.DBusSettingsMethod{dbusName=name, dbusFormat=C.FormatSettings{formatFullText=format, formatColor=color, formatMarkup}} = do
    client <- connectSession
    let goodName = "org.bulby.HaskellStatus." ++ name  
    nameStatus <- requestName client (fromString goodName) [nameDoNotQueue] 
    if nameStatus /= NamePrimaryOwner then 
        ioError $ userError "couldn't obtain name"
    else 
        do
            let method = autoMethod "Update" callback
            export client (fromString $ '/':replace "." "/" goodName)
                defaultInterface { interfaceName = fromString goodName 
                                  , interfaceMethods = 
                                 [ method ] 
                                 }
             
    where 
    callback :: String -> IO () 
    callback input = do 
        sysinfo <- takeMVar sysinfoMvar
         
        let sysinfo' = sysinfo { C.sysinfDbus = HM.insert (toString name) 
            C.Block{
                blockFullText = T.pack (fromString (T.unpack format) ~~ input),
                blockColor    = color ,
                blockMarkup   = formatMarkup
            } 
            (C.sysinfDbus sysinfo) }
        
        putStrLn $ formatGeneral (fromString $ toString (C.settingsFormat settings)) sysinfo'
        putMVar mvar sysinfo'
setupClient settings C.DBusSettingsSignal{dbusPath=path, dbusSignal=signal, dbusFormat=format, dbusName = name} mvar = do 
    let matchRule = matchAny  { matchInterface = Just $ fromString path, matchMember = Just $ fromString signal}
    addMatch signalClient matchRule cb 
    pure () 
    where 
    cb :: Signal -> IO () 
    cb Signal{signalBody=body} = 
        if null body then 
            pure () 
        else do 
            sysinfo <- takeMVar mvar 

            let sysinfo' = sysinfo {C.sysinfDbus = HM.insert name (fromString format ~~ showValue True (varToVal (head body))) (C.sysinfDbus sysinfo) } 
            putStrLn $ formatGeneral (fromString $ toString (C.settingsFormat settings)) sysinfo' 
            putMVar mvar sysinfo'
        

setupClients :: C.Settings -> MVar C.SystemInfo -> IO () 
setupClients conf@C.Settings{settingsDbus=dbuses} mvar = 
    traverse_ (\x ->setupClient conf x mvar) dbuses 
defaultMap :: C.Settings -> HM.HashMap String C.DBusSettings
defaultMap C.Settings{settingsDbus=dbuses} = 
    HM.fromList $ map (\v -> (toString $ C.dbusName v, v)) dbuses
emptyMap :: C.Settings -> HM.HashMap String String 
emptyMap = 
    HM.map (const "") . defaultMap 
