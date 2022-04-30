{-# LANGUAGE FlexibleInstances #-}
module Status.Plugins.BatteryInfo (BatteryInfo(..), BatteryState(..), batteryInfo, showBatteryInfo, fetchBatteryInfo, estimateSecondsRemaining) where 

import Data.List 
import Data.Maybe (isJust, fromMaybe, maybe, fromJust)
import Data.Bool (bool) 
import Control.Applicative (liftA2, (<|>))
import Data.List.Extra (trim)
import Status.Config
import qualified Data.Map.Strict as M
import Status.Helpers
import Text.Format qualified as F
import Text.Format ((~~), (~%))
import Data.Text qualified as T
import Data.String (fromString)
import Data.Monoid (Last(..))
import Data.Functor.Identity 
import Data.Text.IO qualified as TIO
import Data.Time.Clock qualified as Clock 
import Data.Time.Format qualified as CF
import Status.Display 
import Data.Functor ((<&>))
data BatteryState = Discharging | Charging | Full | Unknown deriving Show 

parseBatteryState = \case 
    "Charging" -> Charging 
    "Discharging" -> Discharging 
    "Not charging" -> Discharging 
    "Full" -> Full 
    _ -> Unknown
    
    
data BatteryInfo = BatteryInfo
    {bFullDesign :: Maybe Int
    ,bFullLast :: Maybe Int
    ,bRemaining :: Maybe Int
    ,bPresentRate :: Maybe Int
    ,bSecondsRemaining :: Maybe Int
    ,bCapacity :: Maybe Float 
    ,bStatus :: BatteryState 
    } deriving Show
batteryInfo :: BatterySettings -> IO BatteryInfo
batteryInfo config = do 
    let 
        path = batteryPath config
        statusPrefix = "POWER_SUPPLY_STATUS=" 
        remainingPrefixWatt = "POWER_SUPPLY_ENERGY_NOW=" 
        remainingPrefixAmp  = "POWER_SUPPLY_CHARGE_NOW="
        capacityPrefix = "POWER_SUPPLY_CAPACITY=" 
        currentPrefix = "POWER_SUPPLY_CURRENT_NOW=" 
        voltagePrefix = "POWER_SUPPLY_VOLTAGE_NOW=" 
        timeToEmptyPrefix = "POWER_SUPPLY_TIME_TO_EMPTY_NOW=" 
        powerPrefix = "POWER_SUPPLY_POWER_NOW=" 
        fullDesignWattsPrefix = "POWER_SUPPLY_ENERGY_FULL_DESIGN="
        fullDesignAmpPrefix = "POWER_SUPPLY_CHARGE_FULL_DESIGN="
        lastFullWattPrefix = "POWER_SUPPLY_ENERGY_FULL=" 
        lastFullAmpPrefix = "POWER_SUPPLY_CHARGE_FULL="         
    daData <- lines <$> readFile (T.unpack path)
    let 
        status = maybe Unknown parseBatteryState (findAndDropPrefix statusPrefix daData)
        wattRem = findDropRead remainingPrefixWatt daData 
        isWatt = isJust wattRem 
        remaining = case wattRem of 
                        Just _ -> wattRem  
                        Nothing -> findDropRead remainingPrefixAmp daData :: Maybe Int
        capacity = if isJust remaining then Nothing else findDropRead capacityPrefix daData :: Maybe Float
        presentRate = abs <$> if isWatt then findDropRead powerPrefix daData else findDropRead currentPrefix daData :: Maybe Int 
        voltage = abs <$> findDropRead voltagePrefix daData :: Maybe Int
        secondsRem = findDropRead timeToEmptyPrefix daData :: Maybe Int 
        fullDesign = if isWatt then findDropRead fullDesignWattsPrefix daData else findDropRead fullDesignAmpPrefix daData :: Maybe Int
        fullLast = findDropRead (bool lastFullAmpPrefix lastFullWattPrefix isWatt) daData :: Maybe Int
    if isWatt then 
        pure $ BatteryInfo fullDesign fullLast remaining presentRate secondsRem capacity status
    else 
       pure $ case voltage of 
            Just n -> 
                let ampToWatt' = ampToWatt n in BatteryInfo (ampToWatt' <$> fullDesign) (ampToWatt' <$> fullLast) (ampToWatt' <$> remaining) (ampToWatt' <$> presentRate) secondsRem capacity status  
            Nothing -> BatteryInfo Nothing Nothing Nothing Nothing secondsRem capacity status 
    where 
        findAndDropPrefix str daData = drop (length str) <$> find (isPrefixOf str) daData
        findDropRead str daData = read <$> findAndDropPrefix str daData
        ampToWatt voltage current = round $ (fromIntegral voltage / 1000) * (fromIntegral current / 1000)
showBatteryInfo :: BatterySettings -> BatteryInfo -> Either T.Text T.Text
showBatteryInfo config@(BatterySettings
            {batteryUselast=useLast
            ,batteryFormatDown=FormatSettings{formatText=unknownText}
            ,batteryFormatPrecision=precision 
            ,batteryFormat=FormatSettings{
               formatText=formatConf
            }})
        (BatteryInfo fullDesign lastFull remaining presentRate rawSecondsRem rawCapacity status) =  
    let
        full = biasedAlt useLast fullDesign lastFull
        capacity = (*100) <$> (clamp (0, 1) <$> (rawCapacity <|> ((/) <$> (fromIntegral <$> remaining) <*> (fromIntegral <$> full))))
        secondsRem = rawSecondsRem <|> estimateSecondsRemaining presentRate full remaining status
        health = (/) <$> (fromIntegral <$> lastFull) <*> (fromIntegral <$> fullDesign)
    in 
        -- this was busted because remaining wasn't right var
        case (full, secondsRem, capacity) of 
            (Nothing, Nothing, Nothing) -> Left unknownText
            _ -> Right $ batteryDisplay config capacity status secondsRem

fetchBatteryInfo config = showBatteryInfo config <$> batteryInfo config

biasedAlt True  l r = r <|> l 
biasedAlt False l r = l <|> r

displayStatus config Charging    = batteryStatusCharging    config 
displayStatus config Discharging = batteryStatusDischarging config 
displayStatus config Full        = batteryStatusFull        config
displayStatus config Unknown     = batteryStatusUnknown     config

batteryDisplay :: BatterySettings -> Maybe Float -> BatteryState -> Maybe Int -> T.Text
batteryDisplay config@BatterySettings
                                {batteryFormat=FormatSettings{formatText=daFormat} 
                                ,batteryFormatPrecision=precision
                                } percentage status remaining =  
    fromString (T.unpack daFormat) ~~ ("percentage" ~% (maybe "??" (show . roundTo precision) percentage ++ "%")) ~~ ("status" ~% displayStatus config status) ~~ ("remaining" ~% maybe "XX:XX" formatSeconds remaining)

estimateSecondsRemaining :: Maybe Int -> Maybe Int -> Maybe Int -> BatteryState -> Maybe Int
estimateSecondsRemaining (Just presentRate) (Just full) (Just rem) Charging = 
    Just $ round $ 3600 * fromIntegral (full - rem) / fromIntegral presentRate 
estimateSecondsRemaining (Just presentRate) _ (Just rem) Discharging = 
    Just $ round $ 3600 * fromIntegral rem / fromIntegral presentRate
estimateSecondsRemaining _ _ _ _ = Nothing

formatSeconds sec = 
    CF.formatTime CF.defaultTimeLocale "%h:%0M:%0S" (fromIntegral sec :: Clock.DiffTime) 
instance Processor BatterySettings where 
    process a@BatterySettings{
                batteryFormat,
                batteryFormatDown}= 
        do  
            info <- fetchBatteryInfo a
            pure $ case info of
                Left x -> 
                    processFilledFormat x batteryFormatDown
                Right x -> 
                    processFilledFormat x batteryFormat
