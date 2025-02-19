{-# LANGUAGE ParallelListComp #-}
import Data.Bits
import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace
import Control.Monad (replicateM_, forM_, forever)


newtype Map c = M [[[[c]]]]
    deriving (Functor, Foldable, Show)
-- main:: IO ()
-- main= dim1mineSweeper

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


-- dim1mineSweeper :: IO ()
-- dim1mineSweeper = do
--     n ::Int  <- ioint
--     n1::Int <- ioint
--     let inputlist = []
--     let arr =  dim1arr n n1
--     repeatSweeper arr inputlist

-- ioint :: IO Int
-- ioint = read <$> getLine

-- repeatSweeper :: [Int] -> [Int] -> IO ()
-- repeatSweeper arr inputlist = do
--     print $ hiddenDim1arr (map show arr) inputlist
--     if length inputlist < length arr - mineCounter arr then print "continue" else print "you lucky"
--     input ::Int <- ioint
--     if isMine input arr  then error $ "\nyou doomed answer is " ++ show arr
--     else do
--         let l = input : inputlist
--         repeatSweeper arr l


mineCounter :: Map Int -> Int
mineCounter = sum

isMine :: (Int, Int, Int, Int) -> Map Int -> Bool
isMine (x,y,z,w) (M map) = 1 == ((((map !! max 0 w) !! max 0 z) !! max 0 y) !! max 0 z)

-- nslist :: [(Int, Int, Int, Int)] -> [[[[String]]]] -> Map Int
-- nslist ((x,y,z,w):ls) list nn =
    
--                             --  (([if a == b then x else 0 | a <- list | b <- repeat x | c <- repeat y | d <- repeat z | e <- repeat w ]))

-------------------todo: i should ride 5516
-----------code 가 날아갈까봐 무서워서 커밋함 

hiddenDim1arr :: Map String -> Map Int -> Map String
hiddenDim1arr a s= M [[[[]]]]
-- hiddenDim1arr arr ns = 
--        [if x /= y then ['*'] else fromMaybe "*" (arr!? (read x::Int)) | x <- map show [0..] | y <- nslist  ns (length arr)]

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