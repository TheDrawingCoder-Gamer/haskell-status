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
setupClient :: C.Settings -> C.DBusSettings -> MVar C.SystemInfo -> IO ()
setupClient settings C.DBusSettings{dbusName=name, dbusFormat=format} mvar = do
    client <- connectSession
    let goodName = "org.bulby.HaskellStatus." ++ toString name  
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
        sysinfo <- takeMVar mvar
         
        let sysinfo' = sysinfo { C.sysinfDbus = HM.insert (toString name) (fromString (toString format) ~~ input) (C.sysinfDbus sysinfo) }
        
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
