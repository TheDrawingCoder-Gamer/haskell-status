{-# LANGUAGE ImportQualifiedPost, TypeApplications, OverloadedStrings, NamedFieldPuns #-}
module Status.Plugins.WirelessInfo where

import System.Process qualified as P
import System.Linux.Netlink hiding (query)
import System.Linux.Netlink.GeNetlink.NL80211.Constants 
import System.Linux.Netlink.GeNetlink.NL80211
import System.Linux.Netlink.GeNetlink.NL80211.StaInfo
import Data.Map.Strict qualified as M
import Control.Monad.Trans.Maybe
import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as BS8
import Status.Helpers
import Data.Int (Int8(..))
import Data.Serialize.Get (runGet) 
import Data.Serialize.Put (runPut, putWord32host, putByteString)
import Control.Monad.IO.Class
import System.Posix.IO (closeFd)
import Data.Maybe (listToMaybe, fromMaybe, maybe, isJust)
import Data.Functor((<&>))
import Data.String
import Text.Format qualified as F 
import Text.Format ((~~), (~%))
import Data.Functor.Identity
import Status.Config
import Data.List (find)
import Data.List.Extra (trim)
import Data.Text qualified as T
import Data.Monoid (Any(..))
import Network.Info qualified as NI
import Status.Display 
getName :: IO String 
getName = 
    trim <$> P.readProcess "iwgetid" ["-r"] []
getBssid = 
    trim <$> P.readProcess "iwgetid" ["-ar"] []
data NetworkInfo = NetworkInfo 
    { wiSsid :: T.Text 
    , wiSignal :: Int
    , wiQlty :: Int
    , wiIpv4 :: NI.IPv4 
    , wiIpv6 :: Maybe NI.IPv6
    } deriving Show
getNetworkData :: T.Text -> IO (Maybe NetworkInfo) 
getNetworkData ifname = 
    bracket makeNL80211Socket (closeFd . getFd) (\sock -> do
        iflist <- getInterfaceList sock 
        runMaybeT $ do   
            ifidx <- MaybeT . pure $ foldr (\(n, i) z -> if ifname == "" || T.unpack ifname == n then Just i else z) Nothing iflist 
            scanp <- liftIO (getConnectedWifi sock ifidx) >>= 
                MaybeT . pure . listToMaybe
            bssid <- MaybeT . pure $ M.lookup eNL80211_ATTR_BSS (packetAttributes scanp) >>= 
                        rightToMaybe . runGet getAttributes >>= 
                        M.lookup eNL80211_BSS_BSSID 
            stap <- liftIO (query sock eNL80211_CMD_GET_STATION True $ M.fromList
                            [(eNL80211_ATTR_IFINDEX, runPut $ putWord32host ifidx)
                            ,(eNL80211_ATTR_MAC, runPut $ putByteString bssid)]) >>= 
                    MaybeT . pure . listToMaybe
            interfaces <- liftIO NI.getNetworkInterfaces 
            infoface <- MaybeT . pure $ find (\x -> NI.name x == T.unpack ifname) interfaces :: MaybeT IO NI.NetworkInterface 
            ssid <- MaybeT . pure $ getWifiAttributes scanp >>= M.lookup eWLAN_EID_SSID <&> BS8.unpack <&> T.pack
            signal <- MaybeT . pure $ staInfoFromPacket stap >>= staSignalMBM <&> fromIntegral @Int8 . fromIntegral 
            let 
                qlty = round @Float . xbmToPercent $ signal
                ipv4 = NI.ipv4 infoface 
                ipv6 = NI.ipv6 infoface
            MaybeT . pure $ Just $ NetworkInfo ssid signal qlty ipv4 (Just ipv6)
        
            )
    where 
        rightToMaybe = either (const Nothing) Just
     
getDisplayWirelessInfo :: WirelessSettings -> IO (Either T.Text T.Text)
getDisplayWirelessInfo config@WirelessSettings{wiInterface} = do 
    daData <- getNetworkData wiInterface
    let 
        wrapper = if isJust daData then Right else Left
    pure . wrapper $ displayWireless config daData 
xbmToPercent =
    let 
        spanning = 70
        spanningDiv = spanning / 100 -- the lowest power of 10 greater than spanning
        lowbound = -90 
        upbound  = lowbound + spanning
    in
    (100 -) . (upbound -) . clamp (lowbound, upbound) . fromIntegral 
        
headMaybe [] = Nothing 
headMaybe (x:xs) = Just x
displayWireless :: WirelessSettings -> Maybe NetworkInfo -> T.Text
displayWireless WirelessSettings{wiFormatDown=FormatSettings{formatText}} Nothing = formatText
displayWireless 
    WirelessSettings
        {wiFormat=FormatSettings{formatText=format}
        ,wiPrecision=precision} 
             
    (Just (NetworkInfo {wiSsid=ssid 
                ,wiQlty=qlty 
                ,wiIpv4=ipv4
                ,wiIpv6=ipv6
                })) = 
     fromString (T.unpack format )
     ~~ ("strength" ~% (show qlty ++ "%")) 
     ~~ ("ssid"     ~% ssid)
     ~~ ("ip"       ~% show ipv4)
     ~~ ("ipv6"     ~% show ipv6)

instance Processor WirelessSettings where 
    process conf@WirelessSettings
            {wiFormat=FormatSettings{formatColor=uColor, formatMarkup=uMarkup}
            ,wiFormatDown=FormatSettings{formatColor=dColor, formatMarkup=dMarkup}} = do
        daText <- getDisplayWirelessInfo conf
        pure $ case daText of 
            Left x -> 
                Block x dColor dMarkup 
            Right x -> 
                Block x uColor uMarkup
        
