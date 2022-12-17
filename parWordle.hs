import qualified Data.Set as Set
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
      mapM_ print $ filter (\x -> B.length x == 5) (B.words f)