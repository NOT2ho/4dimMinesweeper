import Data.Bits
import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace
import Control.Monad (replicateM_, forM_, forever)


newtype Map c = M [[[[c]]]]
    deriving (Functor, Foldable, Show)
    
main:: IO ()
main= dim4MineSweeper

rowColumnChooser:: IO String
rowColumnChooser = getLine

xorshift32 :: (Num a, Bits a) => a -> a
xorshift32 seed =
    let l13seed = seed .^. (seed .<<. 13) in
    let r17seed = l13seed .^. (l13seed .>>. 17) in
    let l5seed = r17seed .^. (r17seed .<<. 5) in
        l5seed .&. 0xFFFFFFFF

xorshift32inf :: Int -> [Float]
xorshift32inf seed= map ((/ 4294967295) . fromIntegral) $ iterate xorshift32 seed


zeroMaker :: Int -> Int -> Int -> Int -> [[[[Int]]]]
zeroMaker x y z w = replicate w (replicate z (replicate y ( replicate x 0 )))


mapMaker :: Float -> Int -> Int -> Int -> Int -> Int -> Map Int
mapMaker diff seed x y z w =
    let list = map ((\a -> if round a `rem` round diff == 0 then 1 else 0) . (* 10)) $ xorshift32inf seed in
    M $ split x $ split y (split z (take (x*y*z*w) list))

split :: Int -> [a] -> [[a]]
split _ [] = []
split n l
  | n > 0 = take n l : split n (drop n l)
  | otherwise = error "user problem"


dim4MineSweeper :: IO ()
dim4MineSweeper = do
    putStr "x: "
    x ::Int  <- ioint
    putStr "y: "
    y ::Int  <- ioint
    putStr "z: "
    z ::Int  <- ioint
    putStr "w: "
    w ::Int  <- ioint
    putStr "difficulty: "
    let size = x*y*z*w
    diff::Float <- fromIntegral <$> ioint
    putStr "random seed: "
    rand::Int  <- ioint
    let inputlist = []
    let arr =  mapMaker diff rand x y z w
    repeatSweeper arr inputlist size

ioint :: IO Int
ioint = read <$> getLine

repeatSweeper ::  Map Int-> [(Int, Int, Int, Int)] ->  Int -> IO ()
repeatSweeper arr inputlist size = do
    print $ hiddenDim4arr inputlist arr
    if length inputlist < size - mineCounter arr then print "continue" else print "you lucky"
    putStr "next x: "
    input ::Int <- ioint
    putStr "next y: "
    input1 ::Int <- ioint
    putStr "next z: "
    input2 ::Int <- ioint
    putStr "next w: "
    input3 ::Int <- ioint
    let inputTuple = (input, input1, input2, input3)
    if isMine inputTuple arr  then error $ "\nyou doomed answer is " ++ show arr
    else do
        let l = inputTuple : inputlist
        repeatSweeper arr l size


mineCounter :: Map Int -> Int
mineCounter = sum

isMine :: (Int, Int, Int, Int) -> Map Int -> Bool
isMine (x,y,z,w) (M map) = 1 == ((((map !! max 0 w) !! max 0 z) !! max 0 y) !! max 0 z)

nslist :: [(Int, Int, Int, Int)] -> Map a -> Map (Int, Int, Int, Int)
nslist tuple (M arr) =
    let w =length arr
    in let z = length $ head arr
    in let y =length (head (head arr))
    in let x =length (head (head (head arr)))
    in
       M [[[[ if (a,b,c,d) `elem` tuple then (a,b,c,d) else (-1,-1,-1,-1) | a <- [0..w]] | b <- [0..z]] | c <- [0..y]] | d <- [0..x]]


idxListConstruct :: Map Int -> Map (Int, Int, Int, Int)
idxListConstruct (M arr) =
    let w =length arr
    in let z = length $ head arr
    in let y =length (head (head arr))
    in let x =length (head (head (head arr)))
    in
       M [[[[ (a,b,c,d) | a <- [0..w]] | b <- [0..z]] | c <- [0..y]] | d <- [0..x]]


hiddenDim4arr :: [(Int, Int, Int, Int)] -> Map Int -> Map String
hiddenDim4arr tuple (M arr) =
    let w =length arr
    in let z = length $ head arr
    in let y =length (head (head arr))
    in let x =length (head (head (head arr)))
    in
       M [[[[ if (a,b,c,d) `elem` tuple then show ((((arr !! max 0 d) !! max 0 c) !! max 0 b) !! max 0 a)  else "*" | a <- [0..w]] | b <- [0..z]] | c <- [0..y]] | d <- [0..x]]



numDim4arr :: Map Int -> Map Int
numDim4arr (M arr) =
    let w =length arr
    in let z = length $ head arr
    in let y =length (head (head arr))
    in let x =length (head (head (head arr)))
    in
    let xp = map (map (map (0:))) arr
    in let xm = map (map (map tail)) arr
    in let yp = map (map (replicate x 0 :) ) arr
    in let ym = map (map tail) arr
    in let zp = map ((replicate y $ replicate x 0) :) arr
    in let zm = map tail arr
    in let wp = replicate z (replicate y $ replicate x 0) : arr
    in let wm = tail arr
    in
        M (foldl1 elemwiseAdd [xp, xm, yp, ym, zp, zm, wp, wm])


elemwiseAdd :: [[[[Int]]]] ->  [[[[Int]]]] -> [[[[Int]]]]
elemwiseAdd = zipWith (zipWith (zipWith (zipWith (+))))

(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n >= 0 && n < length xs = Just (xs !! n)
    | otherwise = Nothing