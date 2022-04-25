{-# LANGUAGE OverloadedStrings, DerivingVia, DeriveGeneric, TypeFamilies, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, DataKinds, MultiParamTypeClasses, TypeApplications #-}
{-# LANGUAGE 
 ScopedTypeVariables
,FlexibleContexts
,PolyKinds
,TypeOperators
,ConstraintKinds
,DisambiguateRecordFields 
 #-}
module Status.Config {- (
    PartialSettings(..),
    Settings(..), 
    BatterySettings'(..), 
    BatterySettings, 
    PartialBatterySettings,
    CpuSettings'(..), 
    CPUSettings, 
    PartialCPUSettings,
    MemoryCalcMethod(..), 
    MemorySettings'(..),
    MemorySettings,
    PartialMemorySettings,
    WirelessSettings'(..), 
    WirelessSettings, 
    PartialWirelessSettings, 
    ClockSettings(..),
    CClockSettings, 
    PartialClockSettings,
    getConfig,
    completeSettings,
    ) -}where 

import Toml (TomlCodec, (.=))
import Status.Units
import qualified Toml
import System.Environment
import Data.Maybe (fromJust, mapMaybe, catMaybes,isNothing,isJust)
import Data.Monoid (Last(..))
import Data.Functor.Identity 
import Generic.Data (Generic, Generically(..))
import Data.List (isPrefixOf,find) 
import Data.Text qualified as T
import Data.Typeable (Typeable, Proxy(..))
import Data.Kind (Type)
import GHC.TypeLits
import GHC.Generics (Rep)
import GHC.Generics qualified as G
import Data.Char (toLower, isLower)
import Control.Category qualified as C
import Data.HashMap.Strict qualified as HM
import Generics.OneLiner (Constraints)
data SystemInfo = SystemInfo
    { sysinfBattery  :: Maybe String 
    , sysinfCpu      :: Maybe String 
    , sysinfMemory   :: Maybe String 
    , sysinfWireless :: Maybe String
    , sysinfClock    :: Maybe String 
    , sysinfAudio    :: Maybe String 
    , sysinfFifo     :: HM.HashMap String String
    } 
data SystemMask = SystemMask 
    { sysmaskBattery  :: Bool 
    , sysmaskCpu      :: Bool
    , sysmaskMemory   :: Bool
    , sysmaskWireless :: Bool 
    , sysmaskClock    :: Bool 
    , sysmaskAudio    :: Bool 
    , sysmaskFifo     :: HM.HashMap String Bool }
data Settings' f = Settings
    { settingsFormat :: HKD f T.Text
    , settingsBattery :: !(BatterySettings' f)
    , settingsCpu :: !(CpuSettings' f) 
    , settingsMemory :: !(MemorySettings' f)
    , settingsWireless :: !(WirelessSettings' f)
    , settingsClock :: !(ClockSettings f)
    , settingsAudio :: !(AudioSettings' f)
    , settingsFifo :: ![FIFOSettings]
    } deriving Generic
deriving via (Generically (Settings' f)) instance (Constraints (Settings' f) Semigroup) => Semigroup (Settings' f)
deriving instance (Constraints (Settings' f) Show) => Show (Settings' f)
type Settings = Settings' Identity 
type PartialSettings = Settings' Last 
data BatterySettings' f  = BatterySettings {
    batteryFormat :: HKD f T.Text,
    batteryFormatDown :: HKD f T.Text,
    batteryFormatPrecision :: HKD f Int,
    batteryUselast :: HKD f Bool,
    batteryStatusCharging :: HKD f T.Text,
    batteryStatusDischarging :: HKD f T.Text,
    batteryStatusFull :: HKD f T.Text,
    batteryStatusUnknown :: HKD f T.Text,
    batteryPath :: HKD f T.Text
} deriving stock (Generic)
deriving instance (Constraints (BatterySettings' f) Show) => Show (BatterySettings' f) 
deriving via (Generically (BatterySettings' f)) instance (Constraints (BatterySettings' f ) Semigroup ) => Semigroup (BatterySettings' f)
deriving via (TomlTableStripDot (BatterySettings' f) "battery") instance (Constraints (BatterySettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (BatterySettings' f)  
type BatterySettings = BatterySettings' Identity 
type PartialBatterySettings = BatterySettings' Last

data CpuSettings' f = CpuSettings {
    cpuFormat :: HKD f T.Text,
    cpuFormatDegraded :: HKD f Int,
    cpuFormatPrecision :: HKD f Int 
} deriving stock (Generic) 
deriving via (Generically (CpuSettings' f)) instance (Constraints (CpuSettings' f) Semigroup) => Semigroup (CpuSettings' f)
deriving instance (Constraints (CpuSettings' f) Show) => Show (CpuSettings' f)
deriving via (TomlTableStripDot (CpuSettings' f) "cpu") instance (Constraints (CpuSettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (CpuSettings' f)  
type CPUSettings = CpuSettings' Identity 
type PartialCPUSettings = CpuSettings' Last
data MemorySettings' f  = MemorySettings {
    memCalcMethod :: HKD f MemoryCalcMethod,
    memFormat :: HKD f T.Text,
    memUnit :: HKD f WithAuto,
    memPrecision :: HKD f Int
} deriving stock (Generic) 
deriving via (Generically (MemorySettings' f)) instance (Constraints (MemorySettings' f) Semigroup) => Semigroup (MemorySettings' f)
deriving instance (Constraints (MemorySettings' f) Show) => Show (MemorySettings' f)
deriving via (TomlTableStripDot (MemorySettings' f) "mem") instance (Constraints (MemorySettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (MemorySettings' f)  
type MemorySettings = MemorySettings' Identity 
type PartialMemorySettings =  MemorySettings' Last 
    
data WirelessSettings' f = WirelessSettings
    { wiFormat :: HKD f T.Text 
    , wiFormatDown :: HKD f T.Text 
    , wiPrecision :: HKD f Int
    , wiInterface :: HKD f T.Text 
    } deriving stock Generic 
deriving via (Generically (WirelessSettings' f)) instance (Constraints (WirelessSettings' f) Semigroup) => Semigroup (WirelessSettings' f)
deriving instance (Constraints (WirelessSettings' f) Show) => Show (WirelessSettings' f) 
deriving via (TomlTableStripDot (WirelessSettings' f) "wi") instance (Constraints (WirelessSettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (WirelessSettings' f)
type WirelessSettings = WirelessSettings' Identity 
type PartialWirelessSettings = WirelessSettings' Last

newtype ClockSettings f = ClockSettings
    { clockFormat :: HKD f T.Text }
    deriving stock Generic 
deriving via (TomlTableStripDot (ClockSettings f) "clock") instance (Constraints (ClockSettings f) Toml.HasCodec, Typeable f) => Toml.HasCodec (ClockSettings f)
deriving via (Generically (ClockSettings f)) instance (Constraints (ClockSettings f) Semigroup) => Semigroup (ClockSettings f)
deriving instance (Constraints (ClockSettings f) Show) => Show (ClockSettings f) 
type CClockSettings = ClockSettings Identity 
type PartialClockSettings = ClockSettings Last 

data AudioSettings' f = AudioSettings 
    { audioFormat :: HKD f T.Text 
    , audioFormatMuted :: HKD f T.Text } 
    deriving stock Generic 
deriving via (TomlTableStripDot (AudioSettings' f) "audio") instance (Constraints (AudioSettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (AudioSettings' f) 
deriving via (Generically (AudioSettings' f)) instance (Constraints (AudioSettings' f) Semigroup) => Semigroup (AudioSettings' f)
deriving instance (Constraints (AudioSettings' f) Show) => Show (AudioSettings' f)

type AudioSettings = AudioSettings' Identity 
type PartialAudioSettings = AudioSettings' Last

data FIFOSettings = FIFOSettings
    { fifoPath  :: T.Text 
    , fifoName :: T.Text
    , fifoFormat :: T.Text } deriving stock Generic
    deriving Toml.HasCodec via (TomlTableStripDot FIFOSettings "fifo")
    deriving Toml.HasItemCodec via (TomlTableStripDot FIFOSettings "fifo")  
    deriving Show
    deriving Semigroup via (Generically FIFOSettings)

settingsCodec :: TomlCodec PartialSettings
settingsCodec = Toml.stripTypeNameCodec

getConfig :: IO PartialSettings 
getConfig = 
    do 
        home <- fromJust . lookup "HOME" <$> getEnvironment
         
        Toml.decodeFile settingsCodec (home ++ "/.config/haskell-status/config")

showCalcMeth :: MemoryCalcMethod -> T.Text
showCalcMeth MemAvailable = "memavailable" 
showCalcMeth Classical    = "classical" 

parseCalcMeth :: T.Text -> Either T.Text MemoryCalcMethod 
parseCalcMeth "memavailable" = Right MemAvailable 
parseCalcMeth "classical"    = Right Classical 
parseCalcMeth t              = Left $ T.append "Invalid calc method " t

calcMethCodec = Toml.textBy showCalcMeth parseCalcMeth

completeSettings :: PartialSettings -> Either PartialSettings Settings 
completeSettings input = 
        case complete input of 
            Just c -> 
                Right c
            _ -> Left input

data MemoryCalcMethod 
    = MemAvailable
    | Classical deriving (Enum, Bounded, Show)
instance Toml.HasCodec MemoryCalcMethod where 
    hasCodec = calcMethCodec
instance Toml.HasCodec WithAuto where 
    hasCodec = bytePrefCodec
type family HKD f a where 
    HKD Identity a = a 
    HKD f a = f a  
newtype TomlTableStripPrefix a (s :: Symbol) = TomlTableStripPrefix 
    { unTomlTableStripPrefix :: a } 
newtype TomlTableStripDot a (s :: Symbol) = TomlTableStripDot {unTomlTableStripDot :: a}
instance (Generic a, Toml.GenericCodec (Rep a), KnownSymbol s, Typeable a) => Toml.HasCodec (TomlTableStripPrefix a s) where 
    hasCodec = Toml.diwrap . Toml.table (prefixStripperCodec (Proxy @s) :: Toml.TomlCodec a)
instance (Generic a, Toml.GenericCodec (Rep a), KnownSymbol s, Typeable a) => Toml.HasCodec (TomlTableStripDot a s) where 
    hasCodec = Toml.diwrap . Toml.table (dotPrefixStripCodec (Proxy @s) ::Toml.TomlCodec a)
instance (Generic a, Toml.GenericCodec (Rep a), KnownSymbol s, Typeable a) => Toml.HasItemCodec (TomlTableStripDot a s) where 
    hasItemCodec = Right $ Toml.diwrap (dotPrefixStripCodec (Proxy @s) :: Toml.TomlCodec a)
prefixStripper :: forall s a. (KnownSymbol s, Typeable a) => Proxy s -> Proxy a -> String -> String 
prefixStripper sym _ inp =
    let 
        daPref = symbolVal sym
    in 
        if  daPref `isPrefixOf` inp then 
            headToLower $ drop (length daPref) inp
        else 
            inp

headToLower :: String -> String 
headToLower (x:xs) = toLower x : xs
headToLower _ = error "can't use head to lower on empty list"
prefixStripperOptions :: forall s a. (KnownSymbol s, Typeable a) => Proxy s -> Toml.TomlOptions a
prefixStripperOptions _ = 
    Toml.TomlOptions 
    { tomlOptionsFieldModifier = prefixStripper (Proxy @s) }
prefixStripperCodec :: forall s a. (KnownSymbol s, Typeable a, Generic a, Toml.GenericCodec (Rep a)) => Proxy s -> Toml.TomlCodec a
prefixStripperCodec _ = 
    Toml.genericCodecWithOptions (prefixStripperOptions (Proxy @s))

sepWithDot _ = 
    doIt 
    where 
        doIt str = 
            let 
                (cur, rest) = span isLower str
            in 
                if null rest then 
                    cur 
                else 
                    cur ++ "." ++ doIt (headToLower rest)

sepWithDotOptions :: forall a. (Typeable a) => Toml.TomlOptions a 
sepWithDotOptions = 
    Toml.TomlOptions sepWithDot
sepWithDotCodec :: forall a. (Typeable a, Generic a, Toml.GenericCodec (Rep a)) => Toml.TomlCodec a
sepWithDotCodec = 
    Toml.genericCodecWithOptions sepWithDotOptions
dotPrefixStripCodec :: forall s a. (KnownSymbol s, Typeable a, Generic a, Toml.GenericCodec (Rep a)) => Proxy s -> Toml.TomlCodec a 
dotPrefixStripCodec _ = 
    Toml.genericCodecWithOptions (sepWithDotOptions `composeOptions` prefixStripperOptions (Proxy @s))
composeOptions :: forall a. Toml.TomlOptions a -> Toml.TomlOptions a -> Toml.TomlOptions a 
composeOptions Toml.TomlOptions{tomlOptionsFieldModifier=b} Toml.TomlOptions{tomlOptionsFieldModifier=c} = 
   Toml.TomlOptions (\x -> b x  . c x)

class GComplete last identity where
    gcomplete :: last x -> Maybe (identity x)

instance GComplete last identity => GComplete (G.M1 i c last) (G.M1 i c identity) where
    gcomplete (G.M1 x) = G.M1 <$> gcomplete x

instance GComplete (G.K1 i (Last a)) (G.K1 i a) where
    gcomplete (G.K1 (Last (Just x))) = Just (G.K1 x)
    gcomplete _                    = Nothing
instance GComplete G.U1 G.U1 where
    gcomplete G.U1 = Just G.U1
instance {-# OVERLAPPING #-} GComplete (G.K1 i a) (G.K1 i a) where 
    gcomplete (G.K1 x) = Just $ G.K1 x
instance (GComplete a c, GComplete b d) => GComplete (a G.:*: b) (c G.:*: d) where
    gcomplete (a G.:*: b) = (G.:*:) <$> gcomplete a <*> gcomplete b
instance Complete d => GComplete (G.K1 i (d Last)) (G.K1 i (d Identity)) where
  gcomplete (G.K1 x) = G.K1 <$> complete x
instance Complete d => GComplete (G.K1 i [d Last]) (G.K1 i [d Identity]) where
    gcomplete (G.K1 x) = 
        let 
            
            halfList = map complete x
             
        in 
            if isJust (find isNothing halfList) then 
                Nothing 
            else 
                Just . G.K1 $ catMaybes halfList
             
type Complete d = 
    ( GComplete (Rep (d Last)) (Rep (d Identity))
    , Generic (d Last)
    , Generic (d Identity)
    )

complete :: (Complete d) => d Last -> Maybe (d Identity)
complete = fmap G.to . gcomplete . G.from


