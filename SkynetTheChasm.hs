import System.IO
import Control.Monad
data Road = OK | GAP
data Com = JUMP | SLOW | SPEED | WAIT

data Command = Command Com

instance Show Command where
    show (Command ccd) = 
        case ccd of
            JUMP -> "JUMP"
            SLOW -> "SLOW"
            SPEED -> "SPEED"
            WAIT -> "WAIT"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let r = read input_line :: Int -- the length of the road before the gap.
    input_line <- getLine
    let g = read input_line :: Int -- the length of the gap.
    input_line <- getLine
    let l = read input_line :: Int -- the length of the landing platform.
    let points = calcEndPoints l g
    loop r g l points
    --[Command c | c <- point2commands $ points ]

loop :: Int -> Int -> Int -> [Int] -> IO ()
loop lrBeforeGap lGap lr pnts = do
    input_line <- getLine
    let s = read input_line :: Int -- the motorbike's speed.
    input_line <- getLine
    let x = read input_line :: Int -- the position on the road of the motorbike.

    hPutStrLn stderr . show $ lr
--    hPutStrLn . stderr . show $ (getXNextStep x s) 
    -- A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.
    putStrLn . show .Command $ calcBikeCommand x lrBeforeGap lGap s
    
    loop lrBeforeGap lGap lr pnts

calcBikeCommand :: Int -> Int -> Int -> Int -> Com
calcBikeCommand xx lRoadBeforeGap leGap ssp
-- lbGap
    | xx >= xPosAfterGap = SLOW
    | ssp > leGap + 1 = SLOW
    | ssp < leGap + 1 = SPEED
    | xx == lRoadBeforeGap - 1 = JUMP
    | otherwise = WAIT
    where xPosAfterGap = lRoadBeforeGap + leGap

calcBikePosBeforeGap :: Int -> Int
calcBikePosBeforeGap lroBeforeGap = lroBeforeGap - 1

calcMinSpeedForJump :: Int -> Int
calcMinSpeedForJump lGap1 = lGap1 + 1


calcPoints :: Int -> Int -> [Int] -> [Int]
calcPoints lrBeforeGap11 startSpeed endPonts
    | lrBeforeGap11 > 1 = startSpeed : endPonts
    | otherwise = []
    
calcEndPoints :: Int -> Int -> [Int]
calcEndPoints lrBeforeGap12 lGap12
    | lrBeforeGap12 > 3 = [lrBeforeGap12-lGap12-2,xJumpPoint]
    | lrBeforeGap12 > 2 = [lrBeforeGap12-lGap12-2,xJumpPoint]
    | lrBeforeGap12 > 1 = [xJumpPoint]
    | otherwise = []
    where xJumpPoint = lrBeforeGap12-1

points2commands :: [Int] -> [Com]
points2commands listOfPoint = [SPEED, SPEED, SPEED]

calcMaxSpeedForStop :: Int -> Int
calcMaxSpeedForStop lLP = lLP

getXNextStep:: Int -> Int -> Int
getXNextStep xx ss = xx + ss +1
