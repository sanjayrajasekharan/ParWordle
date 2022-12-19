import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified System.Environment as Env
import qualified System.Exit as Exit

data Word = Word { l1 :: Char
                 , l2 :: Char
                 , l3 :: Char
                 , l4 :: Char
                 , l5 :: Char } | BadGuess deriving Eq

data Knowledge = Knowledge { green :: Set.Set (Int, Char)
                   , yellow :: Set.Set (Int, Char),
                   grey :: Set.Set Char } deriving Eq

initKnowledge :: Knowledge
initKnowledge = Knowledge Set.empty Set.empty Set.empty

main :: IO ()
main = do
   a <- Env.getArgs
   if length a /= 2 then do
      n <- Env.getProgName
      Exit.die $ "Usage: " ++ n ++ " <filename>"
   else do
      let file_name:ans:_ = a
      f <- B.readFile file_name
      let p = (getValidWords . B.words) f
          g = B.pack "crane"
          k = initKnowledge
          w = B.pack ans
      print $ Set.size (possibleWords k p)
      let new_k = addToKnowledge k g w
      print $ Set.size (possibleWords new_k p)


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
matchGreens:: [(Int, Char)] -> Set.Set B.ByteString ->Set.Set B.ByteString
matchGreens [] w = w
matchGreens ((i,c):xs) w = Set.intersection (Set.filter (\x -> B.index x i == c) w) (matchGreens xs w)

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
   