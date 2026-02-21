import Text.Printf
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

type Motif = (Double, Double, Double, Double)

data Mode = Interaction | Protocol | SubGame | MetaGame | Symmetry | Autopoiesis
    deriving (Show, Eq, Ord)

data Agent = Agent 
    { name      :: String
    , threshold :: Double 
    , motif     :: Motif  
    , successes :: Int
    , mode      :: Mode
    } deriving (Show)

adaptiveGain :: Double
adaptiveGain = 45.0

passiveEntropy :: Double
passiveEntropy = 0.02 
nextNoise :: Int -> (Double, Int)
nextNoise seed = 
    let nextSeed = (1103515245 * seed + 12345) `mod` (2^31)
    in (fromIntegral nextSeed / fromIntegral (2^31), nextSeed)

dist4D :: Motif -> Motif -> Double
dist4D (a1,a2,a3,a4) (b1,b2,b3,b4) = 
    sqrt $ (a1-b1)^2 + (a2-b2)^2 + (a3-b3)^2 + (a4-b4)^2

deflect :: Motif -> Motif -> Double -> Motif
deflect (m1,m2,m3,m4) (t1,t2,t3,t4) weight =
    let lerp a b f = a + (b - a) * f
    in (lerp m1 t1 weight, lerp m2 t2 weight, lerp m3 t3 weight, lerp m4 t4 weight)

refine :: Agent -> Motif -> Agent
refine agent targetMotif =
    let d = dist4D (motif agent) targetMotif
    in if d <= threshold agent
       then 
            agent { motif     = deflect (motif agent) targetMotif 0.15 
                  , successes = successes agent + 1
                  , threshold = max 0.05 (threshold agent * 0.92) 
                  , mode      = upgrade (successes agent + 1)
                  }
       else 
            let driftedMotif = deflect (motif agent) (0.5, 0.5, 0.5, 0.5) passiveEntropy
            in agent { motif     = deflect driftedMotif targetMotif (adaptiveGain / 1000.0) 
                     , threshold = min 0.85 (threshold agent * 1.08) 
                     , successes = max 0 (successes agent - 1)
                     }

upgrade :: Int -> Mode
upgrade s
    | s > 40    = Autopoiesis
    | s > 25    = Symmetry
    | s > 15    = MetaGame  
    | s > 8     = SubGame
    | s > 3     = Protocol  
    | otherwise = Interaction

runWatch :: Int -> (Agent, Agent) -> IO ()
runWatch seed (inst, recv) = do
    let newRecv = refine recv (motif inst)
    let d = dist4D (motif inst) (motif recv)
    let (noiseVal, nextSeed) = nextNoise seed
    
    let symbol = if successes newRecv > successes recv then "[✓]" else "[X]"
    printf "%s %-6s -> %-6s | Dist: %.3f | Thr: %.3f | Mode: %-11s | Successes: %d\n" 
           symbol (name inst) (name recv) d (threshold newRecv) (show $ mode newRecv) (successes newRecv)
    hFlush stdout

    threadDelay 1000000
    runWatch nextSeed (newRecv, inst)

main :: IO ()
main = do
    let alpha = Agent "ALPHA" 0.5 (0.1, 0.2, 0.3, 0.4) 0 Interaction
    let beta  = Agent "BETA"  0.5 (0.9, 0.8, 0.7, 0.6) 0 Interaction
    putStrLn "--- AESxARG DRT KERNEL : ENTROPY & DECAY ENABLED ---"
    runWatch 42 (alpha, beta)