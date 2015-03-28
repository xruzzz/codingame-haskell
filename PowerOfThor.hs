import System.IO
import Control.Monad

data Coor = Coor { x, y :: Int}
--data TRIT = (-1) | (0) | (1)
data DIR = DIR { xx, yy :: Int}
getDirX :: Int -> String
getDirX x
    | x > 0  = "E"
    | x < 0  = "W"
    | otherwise = ""

getDirY :: Int -> String
getDirY y
    | y < 0  = "N"
    | y > 0  = "S"
    | otherwise = ""

convertDir2String :: DIR -> String
convertDir2String tDIR = (dirL(yy tDIR)) ++  (getDirX (xx tDIR))
    where dirL = \x -> if x > 0 then "S" else if x < 0 then "N" else ""
       
findDir :: Coor -> Coor -> DIR
findDir thor light = 
    let dx = x light - x thor
        dy = y light - y thor
    in
        DIR { xx = comRes dx, yy = comRes dy}
    where comRes x
            |x > 0 = 1
            |x < 0 = (-1)
            | otherwise = 0


newThorPosition :: Coor -> DIR -> Coor
newThorPosition thorPos thorDir = Coor {x = (x thorPos) + (xx thorDir), y = (y thorPos) + (yy thorDir)}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let lx = read (input!!0) :: Int -- the X position of the light of power
    let ly = read (input!!1) :: Int -- the Y position of the light of power
    let tx = read (input!!2) :: Int -- Thor's starting X position
    let ty = read (input!!3) :: Int -- Thor's starting Y position
    loop Coor { x = tx, y = ty} Coor { x = lx, y = ly}

loop :: Coor -> Coor -> IO ()
loop tt ll = do
    input_line <- getLine
    let e = read input_line :: Int -- The level of Thor's remaining energy, representing the number of moves he can still make.
        dir = findDir tt ll
    -- hPutStrLn stderr "Debug messages..."
    
    -- A single line providing the move to be made: N NE E SE S SW W or NW
    putStrLn (convertDir2String dir)
    loop (newThorPosition tt dir) ll
