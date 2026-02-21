import Text.Printf
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

data DRTPhase = Discernment | Refinement | Taste | Prune
    deriving (Show, Eq)

type Motif = Double
type SNR   = Double

data HGMPlayer = HGMPlayer
    { idTag      :: String
    , dThreshold :: Double
    , rFactor    :: Double
    , state      :: Motif
    , snr        :: SNR   
    , active     :: Bool
    , tasteCount :: Int
    } deriving (Show)

calculateSqueeze :: HGMPlayer -> HGMPlayer -> Double
calculateSqueeze actor target = snr target - dThreshold actor

drtCheck :: HGMPlayer -> HGMPlayer -> (DRTPhase, Double)
drtCheck actor target =
    let entropy = abs (state actor - state target)
    in if snr target < dThreshold actor
       then (Prune, 0.0)
       else if entropy > 0.35
            then (Refinement, entropy * rFactor actor)
            else (Taste, entropy)

reincarnate :: HGMPlayer -> String -> Double -> HGMPlayer
reincarnate old tag newState = HGMPlayer
    { idTag      = tag
    , dThreshold = max 0.45 (dThreshold old * 0.94) 
    , rFactor    = rFactor old
    , state      = newState
    , snr        = 0.85
    , active     = True
    , tasteCount = 0
    }

runDRT :: Int -> (HGMPlayer, HGMPlayer) -> IO ()
runDRT turn (alpha, beta) = do
    let (phase, cost) = drtCheck alpha beta
    let squeeze = calculateSqueeze alpha beta
    
    if phase == Prune
    then do
        putStrLn "--- [SYSTEM] : PRUNING COMPLETED. ---"
        threadDelay 1500000
        runDRT 1 (reincarnate alpha "ALPHA" 0.1, reincarnate beta "BETA" 0.9)
    else do
        let nextBetaSNR = max 0.0 (snr beta - 0.02)
        let (nextAlphaThr, nextTaste) = if phase == Taste 
                                        then (min 0.95 (dThreshold alpha + 0.005), tasteCount alpha + 1) 
                                        else (dThreshold alpha, tasteCount alpha)

        let prevState = state alpha
        let newAlpha = if phase == Refinement 
                       then if state alpha < state beta 
                            then alpha { state = state alpha + 0.06, dThreshold = nextAlphaThr, tasteCount = nextTaste }
                            else alpha { state = state alpha - 0.06, dThreshold = nextAlphaThr, tasteCount = nextTaste }
                       else alpha { dThreshold = nextAlphaThr, tasteCount = nextTaste }

        let internalDelta = abs (state newAlpha - prevState)
        let symbol = case phase of { Taste -> "[*]"; Refinement -> "[~]"; _ -> "[ ]" }

        printf "%s T:%03d | %-10s | THR:%.3f | SNR:%.3f | SQZ:%+5.3f | ΔST:%.3f | TST:%d\n" 
               symbol turn (show phase) (dThreshold newAlpha) (snr beta) squeeze internalDelta (tasteCount newAlpha)
        hFlush stdout

        threadDelay 100000 
        runDRT (turn + 1) (beta, newAlpha)

main :: IO ()
main = do
    let alpha = HGMPlayer "ALPHA" 0.55 0.2 0.1 0.9 True 0
    let beta  = HGMPlayer "BETA"  0.40 0.8 0.8 0.75 True 0
    runDRT 1 (alpha, beta)