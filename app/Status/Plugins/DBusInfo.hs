{-# LANGUAGE FlexibleInstances #-}
module Status.Plugins.DBusInfo where

import DBus.Client
import Control.Concurrent.MVar
import Status.Config qualified as C
import Data.HashMap.Strict qualified as HM
import Data.Stringly
import Data.String
import Data.Foldable (traverse_)
import Data.List.Extra (replace)
import Text.Format ((~~))
import System.IO.Unsafe (unsafePerformIO)
import DBus.Internal.Message (Signal(..))
import DBus.Internal.Types (showValue, varToVal, )
import Data.Text qualified as T
import Data.Text.IsText qualified as T
import Status.Display
import Data.Monoid (Any(..))
import Status.Plugins.WirelessInfo ()
import Status.Plugins.BatteryInfo () 
import Status.Plugins.AudioInfo () 
import Status.Plugins.CpuInfo () 
import Status.Plugins.MemInfo () 
import Status.Plugins.Clock ()
import DBus.Internal.Address (parseAddress)
{-# NOINLINE sysinfoMvar #-}
sysinfoMvar :: MVar C.SystemInfo
sysinfoMvar = unsafePerformIO (newMVar HM.empty)
setupClient :: C.Settings -> C.DBusSettings -> IO ()
setupClient settings C.DBusSettingsMethod{dbusName=name, dbusFormat=C.FormatSettings{formatText=format, formatColor=color, formatMarkup=(Any markup)}, dbusAddress} = do
    client <- connectSpecial dbusAddress
    let goodName = "org.bulby.HaskellStatus." <> name  
    nameStatus <- requestName client (T.fromText goodName) [nameDoNotQueue] 
    if nameStatus /= NamePrimaryOwner then 
        ioError $ userError "couldn't obtain name"
    else 
        do
            let method = autoMethod "Update" callback
            export client (T.fromText $ "/" <> T.replace "." "/" goodName)
                defaultInterface { interfaceName = T.fromText goodName 
                                  , interfaceMethods = 
                                 [ method ] 
                                 }
             
    where 
    callback :: String -> IO () 
    callback input = do 
        sysinfo <- takeMVar sysinfoMvar 
        let sysinfo' = HM.insert name 
                (C.Block{
                blockFullText = T.pack (fromString (T.unpack format) ~~ input),
                blockColor    = color ,
                blockMarkup   = markup
                }) sysinfo      
        displaySysinfo settings sysinfo'
        putMVar sysinfoMvar sysinfo'
setupClient settings C.DBusSettingsSignal{dbusPath=path, dbusSignal=signal, dbusFormat=C.FormatSettings{..}, dbusName = name, dbusObjpath, dbusAddress, dbusUpdate} = do 
    let matchRule = matchAny  { matchInterface = Just $ T.fromText path, matchMember = Just $ T.fromText signal, matchPath = Just $ T.fromText dbusObjpath}
    
    client <- connectSpecial dbusAddress
    addMatch client matchRule cb 
    pure () 
    where 
    cb :: Signal -> IO () 
    cb Signal{signalBody=body} = 
        if null body then 
            pure () 
        else do 
            sysinfo <- takeMVar sysinfoMvar 

            sysinfo' <- updateSysinfo settings dbusUpdate $ HM.insert name C.Block 
                    {C.blockFullText=T.fromText formatText ~~ showValue True (varToVal (head body))
                    ,C.blockColor=formatColor 
                    ,C.blockMarkup=getAny formatMarkup} sysinfo 
            displaySysinfo settings sysinfo' 
            
            putMVar sysinfoMvar sysinfo'
connectSpecial :: T.Text -> IO  Client
connectSpecial addr = 
    case addr of 
        "session" -> connectSession 
        "system"  -> connectSystem 
        "starter" -> connectStarter 
        _         -> maybe (fail "bad address") connect (parseAddress . T.unpack $ addr)
updateSysinfo :: C.Settings -> [T.Text] -> C.SystemInfo -> IO C.SystemInfo 
updateSysinfo C.Settings{..} updates sysinfo = do 
    let doBatt = "battery" `elem` updates
        doCpu  = "cpu"     `elem` updates 
        doMem  = "memory"  `elem` updates 
        doWi   = "wireless" `elem` updates 
        doClck = "clock" `elem` updates 
        doVol  = "audio" `elem` updates 
    coolThing <- sequenceA $ HM.mapMaybe id $ HM.fromList
        [("battery" , if doBatt then Just (process settingsBattery ) else pure <$> HM.lookup "battery"  sysinfo)
        ,("cpu"     , if doCpu  then Just (process settingsCpu     ) else pure <$> HM.lookup "cpu"      sysinfo)
        ,("memory"  , if doMem  then Just (process settingsMemory  ) else pure <$> HM.lookup "memory"   sysinfo) 
        ,("wireless", if doWi   then Just (process settingsWireless) else pure <$> HM.lookup "wireless" sysinfo)
        ,("clock"   , if doClck then Just (process settingsClock   ) else pure <$> HM.lookup "clock"    sysinfo) 
        ,("audio"   , if doVol  then Just (process settingsAudio   ) else pure <$> HM.lookup "audio"    sysinfo)]
    pure $ HM.union coolThing sysinfo
setupClients :: C.Settings -> IO () 
setupClients conf@C.Settings{settingsDbus=dbuses} = 
    traverse_ (setupClient conf ) dbuses 
defaultMap :: C.Settings -> HM.HashMap String C.DBusSettings
defaultMap C.Settings{settingsDbus=dbuses} = 
    HM.fromList $ map (\v -> (toString $ C.dbusName v, v)) dbuses
emptyMap :: C.Settings -> HM.HashMap String String 
emptyMap = 
    HM.map (const "") . defaultMap 
