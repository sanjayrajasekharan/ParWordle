import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Set as List
import qualified Data.ByteString.Char8 as B
import qualified System.Environment as Env
import qualified System.Exit as Exit
import Data.Map (elemAt)

data Word = Word { l1 :: Char
                 , l2 :: Char
                 , l3 :: Char
                 , l4 :: Char
                 , l5 :: Char } | BadGuess deriving Eq

data Knowledge = Knowledge { green :: Set.Set (Int, Char)
                   , yellow :: Set.Set (Int, Char),
                   grey :: Set.Set Char } deriving (Eq, Ord, Show)

initKnowledge :: Knowledge
initKnowledge = Knowledge Set.empty Set.empty Set.empty

main :: IO ()
main = do
   a <- Env.getArgs
   if length a /= 3 then do
      n <- Env.getProgName
      Exit.die $ "Usage: " ++ n ++ "<initialGuess> <answer> <filename>"
   else do
      let guess = head a
      let answer = a !! 1
      if length guess /= 5 || length answer /= 5 then do
         Exit.die $ "Guess and Answer must be length 5"
      else do
         let file_name = a !! 2
         f <- B.readFile file_name
         let p = (getValidWords . B.words) f
             k = initKnowledge
             g = B.pack guess
             w = B.pack answer
         play g w k p
         putStrLn "Done"

play :: B.ByteString -> B.ByteString -> Knowledge -> Set.Set B.ByteString-> IO ()
play g a k p = do
   let newK = addToKnowledge k g a
   let wordSet = possibleWords newK p
   let newGuess = Set.elemAt 0 wordSet --this will be where we have to do minimax
   if newGuess == a then do
      print "Word Found!"
   else do
      print newGuess
      play newGuess a newK p


{- 
Converts a ByteString to data Word. (Currently Not Using)
-}
buildWord:: B.ByteString -> Main.Word
buildWord x
    | B.length x == 5  = Main.Word (B.index x 0) (B.index x 1) (B.index x 2) (B.index x 3) (B.index x 4)
    | otherwise = Main.BadGuess

{-
Converts a set of ByteStrings to a Set of words of length = 5.
-}
getValidWords:: [B.ByteString] -> Set.Set B.ByteString
getValidWords = Set.fromList . filter (\x -> B.length x == 5)

{-
Takes an array of (index, character) and a Set of ByteString. Filters the Set to only include words that
contain character at index.
-}
matchGreens:: Set.Set (Int, Char) -> Set.Set B.ByteString ->Set.Set B.ByteString
matchGreens set w
   | set == Set.null = w
   | otherwise = Set.intersection (Set.filter (\x -> B.index x (set.elemAt 0) == (set.elemAt 1)) w) (matchGreens newSet w)
   where newSet = snd (Set.splitAt 2)


-- matchGreens:: [(Int, Char)] -> Set.Set B.ByteString ->Set.Set B.ByteString
-- matchGreens [] w = w
-- matchGreens ((i,c):xs) w = Set.intersection (Set.filter (\x -> B.index x i == c) w) (matchGreens xs w)
{-
Takes an array of characters and a Set of ByteString. Filters the Set to only include words that
include all characters.
-}
matchYellows:: [(Int, Char)] -> Set.Set B.ByteString -> Set.Set B.ByteString
matchYellows [] w = w
matchYellows ((i, c):xs) w = Set.intersection (Set.intersection (Set.filter (B.elem c) w) (Set.filter (\x -> B.index x i /= c) w)) (matchYellows xs w)

{-
Takes an array of characters and a Set of ByteString. Filters the Set to only include words that
do not include any of the characters.
-}
matchGreys:: String -> Set.Set B.ByteString -> Set.Set B.ByteString
matchGreys xs w
  = foldr (\ x -> Set.intersection (Set.filter (B.notElem x) w)) w xs

{-
Takes a guess and returns a Set of valid possible words.
-}
possibleWords:: Main.Knowledge -> Set.Set B.ByteString -> Set.Set B.ByteString
possibleWords k w = Set.intersection (Set.intersection (matchGreens (Set.toList (green k)) w) (matchYellows (Set.toList (yellow k)) w)) (matchGreys (Set.toList (grey k)) w)

getYellowsHelper:: Int -> B.ByteString -> B.ByteString -> [(Int, Char)]
getYellowsHelper pos g a
   | g == B.empty = []
   | B.head g `B.elem` (x `B.append` B.tail y) = (pos, B.head g): getYellowsHelper (pos+1) (B.tail g) a
   | otherwise = getYellowsHelper (pos+1) (B.tail g) a
   where (x, y) = B.splitAt pos a


-- getYellows:: B.ByteString -> B.ByteString -> [(Int, Char)]
-- getYellows = getYellowsHelper 0

getYellows :: B.ByteString -> B.ByteString -> Set.Set (Int, Char)
getYellows g a = l `Set.difference` getGreens g a
   where s = Set.fromList $ B.unpack a
         l = Set.fromList $ filter (\x -> snd x `Set.member` s) ([0..] `zip` B.unpack g)

getGreens:: B.ByteString -> B.ByteString -> Set.Set (Int, Char)
getGreens g a = Set.fromList ([0..] `zip` B.unpack g) `Set.intersection` Set.fromList ([0..] `zip` B.unpack a)

getGreys:: B.ByteString -> B.ByteString -> Set.Set Char
getGreys g a = s `Set.difference` ng
   where s = Set.fromList $ B.unpack g
         ng = Set.map snd (getGreens g a) `Set.union` Set.map snd (getYellows g a)

addToKnowledge :: Knowledge -> B.ByteString -> B.ByteString -> Knowledge
addToKnowledge k g a = Knowledge greens yellows greys
   where greens = green k `Set.union` getGreens g a
         yellows = yellow k `Set.union` getYellows g a
         greys = grey k `Set.union` getGreys g a

{-
g is the guess, w is the set of possible words at this point
-}

count :: Ord a => [a] -> Map.Map a Float
count = Map.fromListWith (+) . (`zip` repeat 1)

entropy :: B.ByteString -> Set.Set B.ByteString -> Knowledge -> Float
entropy g w k = Map.foldl (+) 0 $ Map.unionWith (*) p inf
   where s = fromIntegral (length w)
         p = Map.map (/s) $ count (map (addToKnowledge k g) (Set.toList w))
         inf = Map.map (\x ->logBase 2 (1/x)) p

entropies :: Set.Set B.ByteString -> Knowledge -> [(B.ByteString, Float)]
entropies w k = e_list
   where w_list = Set.toList w
         e_list = map (\x -> (x, entropy x w k)) w_list

maxEntropyHelper :: (B.ByteString, Float) -> [(B.ByteString, Float)] -> (B.ByteString, Float) 
maxEntropyHelper m []  = m
maxEntropyHelper (max_guess, max_entropy) ((guess, entropy):xs)
   | entropy > max_entropy = maxEntropyHelper (guess, entropy) xs
   | otherwise = maxEntropyHelper (max_guess, max_entropy) xs

maxEntropy :: Set.Set B.ByteString -> Knowledge ->  (B.ByteString, Float) 
maxEntropy w k = maxEntropyHelper (B.empty, 0) (entropies w k)

