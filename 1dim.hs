{-# LANGUAGE ParallelListComp #-}
import Data.Bits
import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace
import Control.Monad (replicateM_, forM_, forever)
main:: IO ()
main= dim1mineSweeper

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

mapMaker :: Int -> Int -> Int -> Int -> [Int]
mapMaker 0 0 seed n = []
mapMaker 0 column seed n=
    let rand = xorshift32 seed in
    (if rand `rem` n == 0 then 1 else 0) : mapMaker 0 (column-1) (xorshift32 seed) n
mapMaker row column seed n =
    let rand = xorshift32 seed in
        if column /= 0 then (if rand `rem` n == 0 then 1 else 0) : mapMaker row (column-1) (xorshift32 seed) n
            else  2 : (if rand `rem` n == 0 then 1 else 0) : mapMaker (row-1) column (xorshift32 seed) n


dim1mineSweeper :: IO ()
dim1mineSweeper = do
    n ::Int  <- ioint
    n1::Int <- ioint
    let inputlist = []
    let arr =  dim1arr n n1
    repeatSweeper arr inputlist

ioint :: IO Int
ioint = read <$> getLine

repeatSweeper :: [Int] -> [Int] -> IO ()
repeatSweeper arr inputlist = do
    print $ hiddenDim1arr (map show arr) inputlist
    if length inputlist < length arr - mineCounter arr then print "continue" else print "you lucky"
    input ::Int <- ioint
    if isMine input arr  then error $ "\nyou doomed answer is " ++ show arr
    else do
        let l = input : inputlist
        repeatSweeper arr l



dim1arr :: Int -> Int -> [Int]
dim1arr n seed = take n $ map ((`rem` 2) . round) $ xorshift32inf seed

mineCounter :: [Int] -> Int
mineCounter = sum

isMine :: Int -> [Int] -> Bool
isMine n list = show (fromMaybe 0 (list!?n) == 1) `trace` fromMaybe 0 (list!?n) == 1

nslist :: [Int] -> Int -> [String]
nslist ns nn =take nn $ map  show $ foldl1 (zipWith (+)) $ map (\t-> [if x == y then x else 0 | x <- [0..] | y <- repeat t]) (nn: ns)


hiddenDim1arr :: [String] -> [Int] -> [String]
hiddenDim1arr arr ns =
       [if x /= y then ['*'] else fromMaybe "*" (arr!? (read x::Int)) | x <- map show [0..] | y <- nslist  ns (length arr)]

numDim1arr :: [Int] -> [Int]
numDim1arr arr = show (foldl1 (zipWith (+)) [tail arr ++ [0], 0 : arr]) `trace` foldl1 (zipWith (+)) [tail arr ++ [0], 0 : arr]

(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n >= 0 && n < length xs = Just (xs !! n)
    | otherwise = Nothing