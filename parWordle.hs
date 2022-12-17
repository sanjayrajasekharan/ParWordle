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

data Guess = Guess { green :: [(Int, Char)]
                   , yellow :: [(Int, Char)],
                   grey :: [Char] } deriving Eq


main :: IO ()
main = do
   a <- Env.getArgs
   if length a /= 1 then do
      n <- Env.getProgName
      Exit.die $ "Usage: " ++ n ++ " <filename>"
   else do
      let file_name:_ = a
      f <- B.readFile file_name
      print ((possibleWords (Guess [(2, 'o'), (0, 'c'), (1, 'h')] [(0, 'o'), (1, 'c'), (3, 'c'), (4, 'r')] ['e', 'a', 'n', 'b', 'l', 'k', 'i']) . getValidWords . B.words) f)

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
matchGreys:: [Char] -> Set.Set B.ByteString -> Set.Set B.ByteString
matchGreys [] w = w
matchGreys (x:xs) w = Set.intersection (Set.filter (B.notElem x) w) (matchGreys xs w)

{-
Takes a guess and returns a Set of valid possible words.
-}
possibleWords:: Main.Guess -> Set.Set B.ByteString -> Set.Set B.ByteString
possibleWords g w = Set.intersection (Set.intersection (matchGreens (green g) w) (matchYellows (yellow g) w)) (matchGreys (grey g) w)
