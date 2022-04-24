{-# LANGUAGE DerivingVia, DeriveGeneric, MultiWayIf, ImportQualifiedPost, OverloadedStrings, RecordWildCards #-}
module Status.Plugins.MemInfo (memUsage, memInfo) where 

import Status.Config
import Data.Maybe (fromJust) 
import Data.List.Extra (trim)
import Data.List (find, isPrefixOf, foldl')
import Status.Units
import Data.Char (isDigit)
import Data.Monoid (Last(..), First(..))
import Status.Helpers 
import Generic.Data (Generic, Generically(..))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Text qualified as T 
import Toml qualified
import Data.String 
import Data.Functor.Identity
import Text.Format ((~~), (~%))
import Text.Format qualified as F
data MemoryInfo = MemoryInfo
    { memTotal     :: First Int
    , memFree      :: First Int 
    , memAvailable :: First Int 
    , memBuffers   :: First Int 
    , memCache     :: First Int 
    , memSwapTotal :: First Int
    , memSwapFree  :: First Int 
    , memSwapCache :: First Int
    } 
    deriving stock (Generic) 
    deriving (Semigroup, Monoid) via (Generically MemoryInfo)

onlyMemTotal n  = 
    let nils = First Nothing in 
    MemoryInfo (First (Just n)) nils nils nils nils nils nils nils
 
onlyMemFree n  = 
    let nils = First Nothing in 
    MemoryInfo nils (First (Just n)) nils nils nils nils nils nils

onlyMemAvailable n  = 
    let nils = First Nothing in 
    MemoryInfo nils nils (First (Just n)) nils nils nils nils nils
 
onlyMemBuffers n  = 
    let nils = First Nothing in 
    MemoryInfo nils nils nils (First (Just n)) nils nils nils nils
 
onlyMemCache n  = 
    let nils = First Nothing in 
    MemoryInfo nils nils nils nils (First (Just n)) nils nils nils
 
onlyMemSwapTotal n  = 
    let nils = First Nothing in 
    MemoryInfo nils nils nils nils nils (First (Just n)) nils nils
 
onlyMemSwapFree n  = 
    let nils = First Nothing in 
    MemoryInfo nils nils nils nils nils nils (First (Just n)) nils

onlyMemSwapCache n  = 
    let nils = First Nothing in 
    MemoryInfo nils nils nils nils nils nils nils (First (Just n))
emptyMemInfo = 
    let 
        nils = First Nothing 
    in 
    MemoryInfo nils nils nils nils nils nils nils nils
memUsage :: Settings -> IO String 
memUsage config = 
    displayMem config <$> memInfo 
displayMem :: Settings -> MemoryInfo -> String
displayMem Settings{
            settingsMemory=
                MemorySettings
                {..}
            } 
           daInfo@MemoryInfo
            {memFree=First (Just free)
            ,memTotal=First (Just total)
            ,memSwapTotal=First (Just swapTotal)
            ,memSwapFree=First (Just swapFree)
            } 
            = 
                let 
                    showPris = showBytes memPrecision    
                    convertUnit = flip convertBytes memUnit 
                    used' = calcUsed daInfo memCalcMethod
                    usedB = convertUnit (Bytes (fromIntegral used') KiB) 
                    freeB = convertUnit (Bytes (fromIntegral free) KiB) 
                    totalB = convertUnit (Bytes (fromIntegral total) KiB) 
                    availB = convertUnit (Bytes (fromIntegral (calcAvailable daInfo memCalcMethod)) KiB)  
                    swapTotalB = convertUnit (Bytes (fromIntegral swapTotal) KiB) 
                    swapFreeB = convertUnit (Bytes (fromIntegral swapFree) KiB) 
                    swapUsedB = convertUnit (Bytes (fromIntegral (calcSwap daInfo)) KiB) 
                    swapAvailB = convertUnit (Bytes (fromIntegral (calcSwapAvailable daInfo)) KiB) 
                    usedPerc = roundTo memPrecision $ (fromIntegral used' / fromIntegral total) * 100
                    freePerc = roundTo memPrecision $ (fromIntegral free / fromIntegral total) * 100
                in
                fromString (T.unpack memFormat) 
                ~~ ("free" ~% showPris freeB)
                ~~ ("available" ~% showPris availB)
                ~~ ("used" ~% showPris usedB) 
                ~~ ("total" ~% showPris totalB) 
                ~~ ("swaptotal" ~% showPris swapTotalB) 
                ~~ ("swapfree" ~% showPris swapFreeB) 
                ~~ ("swapused" ~% showPris swapUsedB) 
                ~~ ("swapavailable" ~% showPris swapAvailB) 
                ~~ ("percentage_used" ~% (show usedPerc ++ "%")) 
                ~~ ("percentage_free" ~% (show freePerc ++ "%"))
displayMem _ _ = error "Incomplete info"
memInfo :: IO MemoryInfo 
memInfo = 
    readFile "/proc/meminfo" <&> foldl' (\a p -> a <> parseLine p) emptyMemInfo . lines
    where 
        parseLine ln = 
            case find (`isPrefixOf` ln) prefixes of 
                Just v -> 
                    if | pMemTotal == v -> onlyMemTotal $ parseGivenPrefix pMemTotal ln
                       | v == pMemFree  -> onlyMemFree $ parseGivenPrefix pMemFree ln 
                       | v == pMemAvailable -> onlyMemAvailable $ parseGivenPrefix pMemAvailable ln
                       | v == pMemBuffers -> onlyMemBuffers $ parseGivenPrefix pMemBuffers ln 
                       | v == pMemCache -> onlyMemCache $ parseGivenPrefix pMemCache ln
                       | v == pMemSwapTotal -> onlyMemSwapTotal $ parseGivenPrefix pMemSwapTotal ln 
                       | v ==pMemSwapFree -> onlyMemSwapFree $ parseGivenPrefix pMemSwapFree ln 
                       | v == pMemSwapCache -> onlyMemSwapCache $ parseGivenPrefix pMemSwapCache ln
                       | otherwise -> undefined 
                Nothing -> 
                    emptyMemInfo
        parseGivenPrefix pre = read . takeWhile isDigit . trim . drop (length pre) 
        pMemTotal = "MemTotal:" 
        pMemFree  = "MemFree:"
        pMemAvailable = "MemAvailable:" 
        pMemBuffers = "Buffers:"
        pMemCache = "Cached:"
        pMemSwapTotal = "SwapTotal:" 
        pMemSwapFree = "SwapFree:" 
        pMemSwapCache = "SwapCached:" 
        -- TODO: THIS IS SO BAD
        prefixes = [pMemTotal, pMemFree, pMemAvailable, pMemBuffers, pMemCache, pMemSwapTotal, pMemSwapFree, pMemSwapCache]

calcUsed memInfo meth = 
    let 
        total' = fromJust . getFirst $ memTotal memInfo 
        available' = calcAvailable memInfo meth
    in 
        total' - available'
calcAvailable memInfo MemAvailable = fromJust . getFirst $ memAvailable memInfo
calcAvailable memInfo Classical    = 
    let 
        free' = fromJust . getFirst $ memFree memInfo 
        buffers' = fromJust . getFirst $ memBuffers memInfo 
        cache' = fromJust . getFirst $ memCache memInfo 
    in 
        free' + buffers' + cache' 
calcSwap memInfo = 
    let 
        total' = fromJust . getFirst $ memSwapTotal memInfo 
        available' = calcSwapAvailable memInfo
    in 
        total' - available'
calcSwapAvailable memInfo = 
    let 
        free' = fromJust . getFirst $ memSwapFree memInfo 
        cache' = fromJust . getFirst $ memSwapCache memInfo 
    in 
        free' + cache'

