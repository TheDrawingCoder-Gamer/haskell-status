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
{-# NOINLINE signalClient #-}
signalClient = unsafePerformIO connectSession
{-# NOINLINE sysinfoMvar #-}
sysinfoMvar :: MVar C.SystemInfo
sysinfoMvar = unsafePerformIO (newMVar HM.empty)
setupClient :: C.Settings -> C.DBusSettings -> IO ()
setupClient settings C.DBusSettingsMethod{dbusName=name, dbusFormat=C.FormatSettings{formatText=format, formatColor=color, formatMarkup=(Any markup)}} = do
    client <- connectSession
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
setupClient settings C.DBusSettingsSignal{dbusPath=path, dbusSignal=signal, dbusFormat=C.FormatSettings{..}, dbusName = name} = do 
    let matchRule = matchAny  { matchInterface = Just $ T.fromText path, matchMember = Just $ T.fromText signal}
    addMatch signalClient matchRule cb 
    pure () 
    where 
    cb :: Signal -> IO () 
    cb Signal{signalBody=body} = 
        if null body then 
            pure () 
        else do 
            sysinfo <- takeMVar sysinfoMvar 

            let sysinfo' = HM.insert name C.Block 
                    {C.blockFullText=T.fromText formatText ~~ showValue True (varToVal (head body))
                    ,C.blockColor=formatColor 
                    ,C.blockMarkup=getAny formatMarkup} sysinfo 
            displaySysinfo settings sysinfo' 

            putMVar sysinfoMvar sysinfo'
        

setupClients :: C.Settings -> IO () 
setupClients conf@C.Settings{settingsDbus=dbuses} = 
    traverse_ (setupClient conf ) dbuses 
defaultMap :: C.Settings -> HM.HashMap String C.DBusSettings
defaultMap C.Settings{settingsDbus=dbuses} = 
    HM.fromList $ map (\v -> (toString $ C.dbusName v, v)) dbuses
emptyMap :: C.Settings -> HM.HashMap String String 
emptyMap = 
    HM.map (const "") . defaultMap 
