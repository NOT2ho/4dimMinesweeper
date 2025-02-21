{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map once" #-}
{-# LANGUAGE ParallelListComp #-}
import Data.Bits
import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace
import Control.Monad (replicateM_, forM_, forever, liftM2, liftM3, liftM4)
import Data.List (permutations)
import Distribution.Compat.Prelude (readMaybe)
import qualified Control.Monad
import Data.Set
    ( Set,
      empty,
      fromList,
      toList,
      insert,
      singleton,
      unions,
      union,
      elemAt,
      notMember,
      null )


newtype Map c = M [[[[c]]]]
    deriving (Functor, Foldable, Show)

main:: IO ()
main= 
    dim4MineSweeper

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
    let list = map ((\a -> if round a `rem` round diff == 0 then 1 else 0) . (* 10000000)) $ xorshift32inf seed in
    M $ split x $ split y (split z (take (x*y*z*w) list))

split :: Int -> [a] -> [[a]]
split _ [] = []
split n l
  | n > 0 = take n l : split n (drop n l)
  | otherwise = error "user problem"

print4DIO :: Map String -> IO ()
print4DIO (M arrrr) = do
    let w =length arrrr
    putStr $ print4D (M arrrr) 0
    putStr "\n\n=========== output END =============\n"
    putStr "== you have to input (or you die) ==\n\n"

print4D :: Map String -> Int -> String
print4D (M (ar:rrr)) n =
     let w =length (ar:rrr)
    in let z = length $ head (ar:rrr)
    in
        if n==w
            then
               "\n============W axis(" ++ show n ++ ")============\n\n\n\n"++ "- - -X axis- - > \n" ++  print3D 0 (z-1) ar
           else
                "\n============W axis(" ++ show n ++ ")============\n\n\n\n"++ "- - -X axis- - > \n"  ++ print3D 0 (z-1) ar
                ++ print4D (M rrr) (n+1)
print4D (M arrrr) n =
    ""




print2D:: [[String]]-> String
print2D arr =concatMap (( ++ "\n") . concatMap (++"|")) arr  ++ "                  ↓ Y axis\n"

print3D :: Int ->  Int ->  [[[String]]] -> String
print3D n n2 (s:ss)
    | n == n2 = "\n-----------Z axis ("
        ++ show n
        ++ ")------------"
        ++ "\n"
        ++ "- - -X axis- - > \n\n\n\n" ++ print2D s
    | n < n2 =  "\n-----------Z axis ("
        ++ show n
        ++ ")------------"
        ++ "\n"
        ++ "- - -X axis- - > \n\n\n\n" ++ print2D s ++ print3D (n+1) n2 ss

dim4MineSweeper :: IO ()
dim4MineSweeper = do
    putStrLn "welcome to the 4 dim minesweeper."
    putStr "x size: "
    x ::Int  <- inputDimSize
    putStr "y size: "
    y ::Int  <- inputDimSize
    putStr "z size: "
    z ::Int  <- inputDimSize
    putStr "w size: "
    w ::Int  <- inputDimSize
    putStr "difficulty(inverse): "
    let size = x*y*z*w
    diff::Float <- fromIntegral <$> inputDimSize
    putStr "random seed: "
    rand::Int  <- inputDimSize
    let inputlist = empty
    let arr =  mapMaker diff rand x y z w
    let numarr = numDim4arr arr
    repeatSweeper numarr inputlist inputlist x y z w arr


inputDimSize :: IO Int
inputDimSize = do
    str <- getLine
    case readMaybe str ::Maybe Int of
        Just i -> if i < 2 then putStrLn "\ntoo small\n" >> inputDimSize else return i
        nothing -> putStrLn "\nint.\n" >> inputDimSize

inputNum :: Int -> IO Int
inputNum n = do
    str <- getLine
    case readMaybe str ::Maybe Int of
        Just i -> if i < 0 || i > n-1 then putStrLn "\nout of index try again\n" >> inputNum n else return i
        nothing -> putStrLn "\nyou did wrong input. do again\n" >> inputNum n


ioint :: IO Int
ioint = read <$> getLine

repeatSweeper ::  Map Int-> Set (Int, Int, Int, Int) -> Set (Int, Int, Int, Int) -> Int -> Int -> Int -> Int -> Map Int -> IO ()
repeatSweeper numarr inputlist totallist x y z w arr = do
    print4DIO $ hiddenDim4arr totallist numarr
    if length totallist < (x*y*z*w) - mineCounter arr then print "continue" else print "you lucky"
    Control.Monad.when (length inputlist == (x*y*z*w) - mineCounter arr) $ print "you win"
    putStr "next x: "
    input ::Int <- inputNum x
    putStr "next y: "
    input1 ::Int <- inputNum y
    putStr "next z: "
    input2 ::Int <- inputNum z
    putStr "next w: "
    input3 ::Int <- inputNum w
    let inputTuple = (input, input1, input2, input3)
    if isMine inputTuple arr  then print4DIO (fmap show arr) >> putStr "\nyou doomed this is answer\n"
    else do
        let l = inputTuple `insert` inputlist
        let totalll = inputTuple `insert` totallist
        putStrLn $ "cell selected by you: " ++ show totalll
        let totall = extender totalll empty numarr
        putStrLn $ "cell extended: " ++ show totall
        repeatSweeper numarr l totall x y z w arr


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
       M [[[[ if (a,b,c,d) `elem` tuple then (a,b,c,d) else (-1,-1,-1,-1) | a <- [0..w-1]] | b <- [0..z-1]] | c <- [0..y-1]] | d <- [0..x-1]]


idxListConstruct :: Map Int -> Map (Int, Int, Int, Int)
idxListConstruct (M arr) =
    let w =length arr
    in let z = length $ head arr
    in let y =length (head (head arr))
    in let x =length (head (head (head arr)))
    in
       M [[[[ (a,b,c,d) | a <- [0..w-1]] | b <- [0..z-1]] | c <- [0..y-1]] | d <- [0..x-1]]


hiddenDim4arr :: Set (Int, Int, Int, Int) -> Map Int -> Map String
hiddenDim4arr tuple (M arr) =
    let w =length arr
    in let z = length $ head arr
    in let y =length (head (head arr))
    in let x =length (head (head (head arr)))
    in
       M [[[[ if (a,b,c,d) `elem` tuple then show ((((arr !! max 0 d) !! max 0 c) !! max 0 b) !! max 0 a) else "*" | a <- [0..w-1]] | b <- [0..z-1]] | c <- [0..y-1]] | d <- [0..x-1]]


extender :: Set (Int, Int, Int, Int) -> Set (Int, Int, Int, Int) -> Map Int -> Set (Int, Int, Int, Int)
extender a opened (M arr) =
    case toList a of
        [] -> opened
        ((x,y,z,w):ls) ->
            let w' = length arr
                in let z' = length $ head arr
                in let y' = length (head (head arr))
                in let x' = length (head (head (head arr)))
                in let opened1 = opened `union` singleton (x, y, z, w)
                in let next1 = fromList
                                    (filter (/= (-1,-1,-1,-1))
                                        [(if not (x+a <0 || y+b <0 || z+c <0 || w+d<0 || x+a >=x' || y+b >=y' || z+c>=z' || w+d>=w')
                                            && notMember (x+a, y+b, z+c, w+d) opened1
                                            then (a+x, y+b, z+c, w+d)
                                            else (-1,-1,-1,-1))
                                        | a <- [-1,0,1] , b <- [-1,0,1] , c <- [-1,0,1] , d <- [-1,0,1] ]
                                    )
                in let opened2 = opened1 `union` next1
                in let next2 = fromList (filter (\(a,b,c,d) -> arr !! max 0 d !! max 0 c !! max 0 b !! max 0 a == 0) (toList next1))
                in extender (fromList ls `union` next2) opened2 (M arr)

numDim4arr :: Map Int -> Map Int
numDim4arr (M arr) =
    let w =length arr
    in let z = length $ head arr
    in let y =length (head (head arr))
    in let x =length (head (head (head arr)))
    in
    let xp = map (map (map (0:)))
    in let xm = map (map (map (tail . (++[0]))))
    in let yp = map (map (replicate x 0 :) )
    in let ym = map (map (tail. (++[replicate x 0])))
    in let zp = map ((replicate y $ replicate x 0) :)
    in let zm = map (tail . (++[replicate y $ replicate x 0]))
    in let wp = (replicate z (replicate y $ replicate x 0) :)
    in let wm = tail . (++ [replicate z (replicate y $ replicate x 0)])
    in let func = [(a <$> b) . c <$> d | a <- [xm, id, xp], b <- [yp, id, ym], c <- [zp, id, zm], d<-[wp,id,wm]]
    in
      M (foldr (elemwiseAdd . ($ arr)) arr func )

elemwiseAdd :: [[[[Int]]]] ->  [[[[Int]]]] -> [[[[Int]]]]
elemwiseAdd = zipWith (zipWith (zipWith (zipWith (+))))

(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n >= 0 && n < length xs = Just (xs !! n)
    | otherwise = Nothing