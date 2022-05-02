{-# LANGUAGE OverloadedStrings, DerivingVia, DeriveGeneric, TypeFamilies, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, DataKinds, MultiParamTypeClasses, TypeApplications #-}
{-# LANGUAGE 
 ScopedTypeVariables
,FlexibleContexts
,PolyKinds
,TypeOperators
,ConstraintKinds
,DisambiguateRecordFields 
,GeneralizedNewtypeDeriving
,DeriveAnyClass
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
import Data.Monoid (Last(..), Any(..))
import Data.Functor.Identity 
import Generic.Data (Generic, Generically(..))
import Data.List (isPrefixOf,find) 
import Data.Text qualified as T
import Data.Typeable (Typeable, Proxy(..))
import Data.Kind (Type, Constraint)
import Data.Coerce (coerce)
import GHC.TypeLits
import GHC.Generics (Rep)
import GHC.Generics qualified as G
import Data.Char (toLower, isLower)
import Control.Category ((<<<), (>>>))
import Control.Category qualified as C
import Data.HashMap.Strict qualified as HM
import Generics.OneLiner (Constraints)
import Control.Applicative ((<|>))
import Control.Lens qualified as Lens
import Text.ParserCombinators.ReadP qualified as P
import Numeric (readHex, showHex)
import Data.Functor ((<&>))

data Block = Block {
    blockFullText :: T.Text, 
    blockColor    :: Maybe Color,
    blockMarkup   :: Bool }
defaultBlock = Block "" Nothing False
type SystemInfo = HM.HashMap T.Text Block
data SystemMask' f= SystemMask
    { sysmaskBattery  :: HKDEither f Any Bool
    , sysmaskCpu      :: HKDEither f Any Bool
    , sysmaskMemory   :: HKDEither f Any Bool
    , sysmaskWireless :: HKDEither f Any Bool
    , sysmaskClock    :: HKDEither f Any Bool
    , sysmaskAudio    :: HKDEither f Any Bool
    , sysmaskDBus     :: HM.HashMap String Bool }
    deriving Generic 
deriving via (Generically (SystemMask' f)) instance (Constraints (SystemMask' f) Semigroup) => Semigroup (SystemMask' f)
deriving via (Generically (SystemMask' f)) instance (Constraints (SystemMask' f) Monoid) => Monoid (SystemMask' f)
type PartialSysMask = SystemMask' Maybe
type SystemMask     = SystemMask' Identity
newtype JustLast a = JustLast {getJustLast :: a }
    deriving newtype Toml.HasCodec
    deriving Show
instance Semigroup (JustLast a) where 
   _ <> b = b 
data DisplayMode 
    = Plain 
    | Swaybar deriving Show 
showDisplayMode Plain = "plain" 
showDisplayMode Swaybar = "swaybar" 

parseDisplayMode "plain" = Right Plain 
parseDisplayMode "swaybar" = Right Swaybar 
parseDisplayMode "i3bar" = Right Swaybar 
parseDisplayMode _      = Left "failed parse display mode"
instance Toml.HasCodec DisplayMode where 
    hasCodec = Toml.textBy showDisplayMode parseDisplayMode 
data Settings' f = Settings
    { settingsBlocks :: !(JustLast [T.Text])
    , settingsMode   :: !(HKD f DisplayMode)
    , settingsBattery :: !(BatterySettings' f)
    , settingsCpu :: !(CpuSettings' f) 
    , settingsMemory :: !(MemorySettings' f)
    , settingsWireless :: !(WirelessSettings' f)
    , settingsClock :: !(ClockSettings f)
    , settingsAudio :: !(AudioSettings' f)
    , settingsDbus  :: ![DBusSettings' f]
    } deriving stock Generic
      deriving anyclass CompleteInstance
deriving via (Generically (Settings' f)) instance (Constraints (Settings' f) Semigroup) => Semigroup (Settings' f)
deriving instance (Constraints (Settings' f) Show) => Show (Settings' f)
type Settings = Settings' Identity 
type PartialSettings = Settings' Last 
data BatterySettings' f  = BatterySettings {
    batteryFormat :: FormatSettings' f,
    batteryFormatDown :: FormatSettings' f,
    batteryFormatPrecision :: HKD f Int,
    batteryUselast :: HKD f Bool,
    batteryStatusCharging :: HKD f T.Text,
    batteryStatusDischarging :: HKD f T.Text,
    batteryStatusFull :: HKD f T.Text,
    batteryStatusUnknown :: HKD f T.Text,
    batteryPath :: HKD f T.Text
} deriving stock (Generic)
  deriving anyclass CompleteInstance
deriving instance (Constraints (BatterySettings' f) Show) => Show (BatterySettings' f) 
deriving via (Generically (BatterySettings' f)) instance (Constraints (BatterySettings' f ) Semigroup ) => Semigroup (BatterySettings' f)
deriving via (TomlTableStripPrefix (BatterySettings' f) "battery") instance (Constraints (BatterySettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (BatterySettings' f)  
type BatterySettings = BatterySettings' Identity 
type PartialBatterySettings = BatterySettings' Last

data CpuSettings' f = CpuSettings {
    cpuFormat :: FormatSettings' f,
    cpuFormatPrecision :: HKD f Int 
} deriving stock (Generic) 
  deriving anyclass CompleteInstance
deriving via (Generically (CpuSettings' f)) instance (Constraints (CpuSettings' f) Semigroup) => Semigroup (CpuSettings' f)
deriving instance (Constraints (CpuSettings' f) Show) => Show (CpuSettings' f)
deriving via (TomlTableStripPrefix (CpuSettings' f) "cpu") instance (Constraints (CpuSettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (CpuSettings' f)  
type CPUSettings = CpuSettings' Identity 
type PartialCPUSettings = CpuSettings' Last
data MemorySettings' f  = MemorySettings {
    memCalcMethod :: HKD f MemoryCalcMethod,
    memFormat :: FormatSettings' f,
    memUnit :: HKD f WithAuto,
    memPrecision :: HKD f Int
} deriving stock (Generic) 
  deriving anyclass CompleteInstance
deriving via (Generically (MemorySettings' f)) instance (Constraints (MemorySettings' f) Semigroup) => Semigroup (MemorySettings' f)
deriving instance (Constraints (MemorySettings' f) Show) => Show (MemorySettings' f)
deriving via (TomlTableStripPrefix (MemorySettings' f) "mem") instance (Constraints (MemorySettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (MemorySettings' f)  
type MemorySettings = MemorySettings' Identity 
type PartialMemorySettings =  MemorySettings' Last 
    
data WirelessSettings' f = WirelessSettings
    { wiFormat :: FormatSettings' f 
    , wiFormatDown :: FormatSettings' f 
    , wiPrecision :: HKD f Int
    , wiInterface :: HKD f T.Text 
    } deriving stock Generic
      deriving anyclass CompleteInstance
deriving via (Generically (WirelessSettings' f)) instance (Constraints (WirelessSettings' f) Semigroup) => Semigroup (WirelessSettings' f)
deriving instance (Constraints (WirelessSettings' f) Show) => Show (WirelessSettings' f) 
deriving via (TomlTableStripPrefix (WirelessSettings' f) "wi") instance (Constraints (WirelessSettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (WirelessSettings' f)
type WirelessSettings = WirelessSettings' Identity 
type PartialWirelessSettings = WirelessSettings' Last

newtype ClockSettings f = ClockSettings
    { clockFormat :: FormatSettings' f }
    deriving stock Generic
    deriving anyclass CompleteInstance 
deriving via (TomlTableStripPrefix (ClockSettings f) "clock") instance (Constraints (ClockSettings f) Toml.HasCodec, Typeable f) => Toml.HasCodec (ClockSettings f)
deriving via (Generically (ClockSettings f)) instance (Constraints (ClockSettings f) Semigroup) => Semigroup (ClockSettings f)
deriving instance (Constraints (ClockSettings f) Show) => Show (ClockSettings f) 
type CClockSettings = ClockSettings Identity 
type PartialClockSettings = ClockSettings Last 

data AudioSettings' f = AudioSettings 
    { audioFormat :: FormatSettings' f 
    , audioFormatMuted :: FormatSettings' f } 
    deriving stock Generic 
    deriving anyclass CompleteInstance
deriving via (TomlTableStripPrefix (AudioSettings' f) "audio") instance (Constraints (AudioSettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (AudioSettings' f) 
deriving via (Generically (AudioSettings' f)) instance (Constraints (AudioSettings' f) Semigroup) => Semigroup (AudioSettings' f)
deriving instance (Constraints (AudioSettings' f) Show) => Show (AudioSettings' f)

type AudioSettings = AudioSettings' Identity 
type PartialAudioSettings = AudioSettings' Last

newtype DefaultString a = DefaultString {unDefault :: Maybe String }
    deriving newtype Show

newtype DefaultText a = DefaultText {unDefaultTxt :: Maybe T.Text} 
    deriving newtype Show

data DBusSettings' f 
    = DBusSettingsMethod
    { dbusFormat  :: Maybe (FormatSettings' f)
    , dbusName    :: T.Text
    , dbusDefault :: Maybe T.Text
    , dbusAddress :: T.Text 
    }
    | DBusSettingsSignal 
    { dbusFormat :: Maybe (FormatSettings' f)
    , dbusName   :: T.Text 
    , dbusPath   :: T.Text
    , dbusObjpath :: T.Text
    , dbusSignal :: T.Text
    , dbusUpdate :: [T.Text]
    , dbusAddress :: T.Text
    }
    deriving stock Generic
    deriving anyclass CompleteInstance
deriving instance (Constraints (DBusSettings' f) Show) => Show (DBusSettings' f)
type DBusSettings = DBusSettings' Identity
dimonoidoptional :: (Monoid m) => Toml.TomlCodec m -> Toml.TomlCodec m
dimonoidoptional Toml.Codec{..} = Toml.Codec 
    { codecRead = codecRead Toml.<!> \_ -> pure mempty
    , codecWrite = codecWrite 
    }
didefault :: Toml.TomlCodec a -> a -> Toml.TomlCodec a 
didefault Toml.Codec{..} def = Toml.Codec 
    { codecRead = codecRead Toml.<!> const (pure def) 
    , codecWrite = codecWrite }
matchDbusMethod s@(DBusSettingsMethod{}) = Just s
matchDbusMethod _                      = Nothing 

matchDbusSignal s@(DBusSettingsSignal{}) = Just s
matchDbusSignal _                      = Nothing

dbusSignalCodec :: forall (a :: Type -> Type). (Typeable a, Constraints (FormatSettings' a) Toml.HasCodec) => Toml.TomlCodec (DBusSettings' a)
dbusSignalCodec = DBusSettingsSignal 
    <$> Toml.table (Toml.dioptional $ prefixStripperCodec (Proxy @"format") :: Toml.TomlCodec (Maybe (FormatSettings' a))) "format" .= dbusFormat 
    <*> Toml.text     "name"   .= dbusName 
    <*> Toml.text "path" .= dbusPath 
    <*> Toml.text "objpath" .= dbusObjpath 
    <*> Toml.text  "signal" .= dbusSignal
    <*> dimonoidoptional (Toml.arrayOf Toml._Text "update") .= dbusUpdate
    <*> didefault (Toml.text "address") "session" .= dbusAddress

dbusMethodCodec :: forall (a :: Type -> Type). (Typeable a, Constraints (FormatSettings' a) Toml.HasCodec) => Toml.TomlCodec (DBusSettings' a)
dbusMethodCodec = DBusSettingsMethod 
    <$> Toml.table (Toml.dioptional $ prefixStripperCodec (Proxy @"format") :: Toml.TomlCodec (Maybe (FormatSettings' a))) "format" .= dbusFormat 
    <*> Toml.text "name" .= dbusName 
    <*> Toml.dioptional (Toml.text "default") .= dbusDefault 
    <*> didefault (Toml.text "address") "session" .= dbusAddress
dbusCodec :: (Typeable f, Constraints (FormatSettings' f) Toml.HasCodec) => Toml.TomlCodec (DBusSettings' f)
dbusCodec =  
        Toml.dimatch matchDbusSignal id
            ( Toml.table dbusSignalCodec "signal" )
    <|>
        Toml.dimatch matchDbusMethod id (Toml.table dbusMethodCodec "method")
data Color = Color { red :: Int, green :: Int, blue :: Int } deriving stock Show 
parseColor :: P.ReadP Color 
parseColor = do 
    P.char '#' 
    red <- P.count 2 P.get
    green <- P.count 2 P.get 
    blue <- P.count 2 P.get 
    P.eof 
    pure $ Color (fst . head $ readHex red) (fst . head $ readHex green) (fst . head $ readHex blue)
parseColorToml = 
    T.unpack <&> P.readP_to_S parseColor <&> \case 
        [(c, "")] -> Right c 
        _         -> Left "Failed parse color"
showColor (Color r g b) = T.pack $ '#':( showHex r . showHex g . showHex b $ "")

instance Toml.HasCodec Color where 
    hasCodec = Toml.textBy showColor parseColorToml 
data FormatSettings' f = FormatSettings 
    { formatText :: HKD f T.Text 
    , formatColor :: Maybe Color 
    , formatMarkup :: HKDIf f Any Bool }
    deriving stock Generic
    deriving anyclass CompleteInstance 
deriving via (Generically (FormatSettings' f)) instance (Constraints (FormatSettings' f) Semigroup) => Semigroup (FormatSettings' f)
deriving instance (Constraints (FormatSettings' f) Show) => Show (FormatSettings' f)
deriving via (TomlTableStripPrefix (FormatSettings' f) "format") instance (Constraints (FormatSettings' f) Toml.HasCodec, Typeable f) => Toml.HasCodec (FormatSettings' f) 
instance Toml.HasCodec (DBusSettings' Last) where 
    hasCodec = Toml.table dbusCodec
instance Toml.HasItemCodec (DBusSettings' Last) where 
    hasItemCodec = Right dbusCodec 
newtype TString = TString { fromTString :: [Char] }
instance Toml.HasCodec TString where
    hasCodec = Toml.diwrap . Toml.string
instance Toml.HasItemCodec TString where 
    hasItemCodec = Left $ Toml._Coerce Toml._String

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
        case ncomplete input of 
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
type family HKDEither f a b where 
    HKDEither Identity a b = b
    HKDEither f a b = f a 
type family HKDIf c a b where 
    HKDIf Identity a b = b 
    HKDIf _ a b = a
newtype TomlTableStripPrefix a (s :: Symbol) = TomlTableStripPrefix 
    { unTomlTableStripPrefix :: a } 
-- newtype TomlTableStripDot a (s :: Symbol) = TomlTableStripDot {unTomlTableStripDot :: a}
instance (Generic a, Toml.GenericCodec (Rep a), KnownSymbol s, Typeable a) => Toml.HasCodec (TomlTableStripPrefix a s) where 
    hasCodec = Toml.diwrap . Toml.table (prefixStripperCodec (Proxy @s) :: Toml.TomlCodec a)
instance (Generic a, Toml.GenericCodec (Rep a), KnownSymbol s, Typeable a) => Toml.HasItemCodec (TomlTableStripPrefix a s) where 
    hasItemCodec = Right $ Toml.diwrap (prefixStripperCodec (Proxy @s) :: Toml.TomlCodec a)
-- instance (Generic a, Toml.GenericCodec (Rep a), KnownSymbol s, Typeable a) => Toml.HasCodec (TomlTableStripDot a s) where 
--    hasCodec = Toml.diwrap . Toml.table (dotPrefixStripCodec (Proxy @s) ::Toml.TomlCodec a)
-- instance (Generic a, Toml.GenericCodec (Rep a), KnownSymbol s, Typeable a) => Toml.HasItemCodec (TomlTableStripDot a s) where 
--    hasItemCodec = Right $ Toml.diwrap (dotPrefixStripCodec (Proxy @s) :: Toml.TomlCodec a)


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

instance GComplete (G.K1 i (Maybe a)) (G.K1 i a) where 
    gcomplete (G.K1 (Just x)) = Just (G.K1 x) 
    gcomplete _               = Nothing

instance GComplete G.U1 G.U1 where
    gcomplete G.U1 = Just G.U1
instance {-# OVERLAPPING #-} GComplete (G.K1 i a) (G.K1 i a) where 
    gcomplete (G.K1 x) = Just $ G.K1 x
instance (GComplete a c, GComplete b d) => GComplete (a G.:*: b) (c G.:*: d) where
    gcomplete (a G.:*: b) = (G.:*:) <$> gcomplete a <*> gcomplete b
instance (GComplete a c, GComplete b d) => GComplete (a G.:+: b) (c G.:+: d) where 
    gcomplete (G.L1 x) = G.L1 <$> gcomplete x
    gcomplete (G.R1 x) = G.R1 <$> gcomplete x
instance NComplete s d => GComplete (G.K1 i s) (G.K1 i d) where 
    gcomplete (G.K1 x) = G.K1 <$> ncomplete x

instance GComplete  (G.K1 i (Maybe Any)) (G.K1 i Bool) where 
    gcomplete (G.K1 (Just (Any x))) = Just $ G.K1 x
    gcomplete _                     = Nothing
class CompleteInstance (d :: (Type -> Type) -> Type) 

class NComplete s d where 
    ncomplete :: s -> Maybe d
class NCompleteInstance (d :: (Type -> Type) -> Type) (a :: (Type -> Type))
instance CompleteInstance d => NCompleteInstance d Last
instance (
            NCompleteInstance d a, 
            GComplete (Rep (d a)) (Rep (d Identity)), 
            Generic (d a), 
            Generic (d Identity)
         ) => NComplete (d a) (d Identity) where 
    ncomplete = fmap G.to . gcomplete . G.from
instance NComplete s d => NComplete [s] [d] where
    ncomplete = Just . mapMaybe ncomplete
instance {-# OVERLAPS #-} NComplete s d => NComplete (Maybe s) (Maybe d) where 
    ncomplete = fmap ncomplete
instance KnownSymbol s => NComplete (DefaultText s) T.Text where 
    ncomplete (DefaultText x) = x <|> Just (T.pack (symbolVal (Proxy @s)))
instance KnownSymbol s => NComplete (DefaultString s) String where 
    ncomplete (DefaultString x) = x <|> Just (symbolVal (Proxy @s))
instance NComplete (Maybe Any) Bool where 
    ncomplete (Just (Any x)) = Just x 
    ncomplete _              = Just False
instance NComplete Any Bool where 
    ncomplete (Any x) = Just x
instance {-# OVERLAPS #-} NComplete (Maybe s) s where 
    ncomplete = id
instance NComplete (Maybe s) d => NComplete (Last s) d where 
    ncomplete (Last x) = ncomplete x
