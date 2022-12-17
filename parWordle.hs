import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified System.Environment as Env
import qualified System.Exit as Exit

main :: IO ()
main = do
   a <- Env.getArgs
   if length a /= 1 then do
      n <- Env.getProgName
      Exit.die $ "Usage: " ++ n ++ " <filename>"
   else do
      let file_name:_ = a
      f <- B.readFile file_name
      print (Map.findWithDefault Set.empty 'a' ((buildMaps . getValidWords . B.words) f !! 4))

getValidWords:: [B.ByteString] -> [B.ByteString]
getValidWords = filter (\x -> B.length x == 5)

buildMap:: [Char] -> [B.ByteString] -> Map.Map Char (Set.Set B.ByteString)
buildMap k x = Map.fromListWith Set.union l
    where l = zip k (map Set.singleton x)

buildMapsHelper:: [B.ByteString] -> [B.ByteString] ->[Map.Map Char (Set.Set B.ByteString)]
buildMapsHelper k x
    | B.length (head k) == 0  = []
    | otherwise = buildMap (map B.head k) x : buildMapsHelper (map B.tail k) x

buildMaps:: [B.ByteString] ->[Map.Map Char (Set.Set B.ByteString)]
buildMaps x = buildMapsHelper x x