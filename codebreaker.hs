-- Codebreaking challenge for West London Hacknight
import           Control.Applicative 
import qualified Data.Set as S
import           Data.List
import           Data.Maybe
import           Data.Char
import           System.Environment
import           System.Exit 
import           System.IO

main :: IO ()
main = do args <- getArgs
          case args of
            [text] -> runDecode text
            _ -> do hPutStrLn stderr "Usage: codebreaker \"<query text>\""
                    exitWith $ ExitFailure 1 

runDecode :: String -> IO ()
runDecode str = do dicttext <- dictionary
                   let dictwords = S.fromList (words dicttext)
                       results = decode str dictwords
                   case results of
                        Nothing -> putStrLn $ "Couldn't decode: " ++ str
                        Just a -> putStrLn $ "Decoded as: " ++ a

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

dictionary :: IO String
dictionary = readFile "/usr/share/dict/words"

decode :: String -> S.Set String -> Maybe String
decode str dict = let combos = (concatMap permutations $ subsequences alldecoders)
                      func = foldl (flip map) $ extractWords str
                      results = map func combos
                      checkdict = check dict
                      best = foldl (\a b -> if isNothing a && checkdict b; then Just b; else a) Nothing results
                  in case best of
                       Nothing -> Nothing
                       Just b -> Just (unwords b)

extractWords :: String -> [String]
extractWords str = words $ filter (\a -> isSeparator a || isLetter a) $ map toLower str

alldecoders :: [String -> String]
alldecoders = decodeOddEven:reverse:(decodeRot <$> [1,2,3,4,5])

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = take (length xs) (drop n (cycle xs))

decodeRot :: Int -> String -> String
decodeRot n str = let newalph = rotate n alphabet
                  in map (\a -> case elemIndex a alphabet of
                                   Just i -> newalph !! i
                                   Nothing -> a) str

decodeOddEven :: String -> String
decodeOddEven str = let len = length str
                        halfway = div (len - mod len 2) 2
                        odds = take halfway str
                        evens = take halfway $ drop halfway str
                        extra = drop (2 * halfway) str
                        in concat (zipWith (\a b -> a:[b]) evens odds) ++ extra


check :: S.Set String -> [String] -> Bool
check dictwords strlist = let allwords = S.fromList strlist
                              numwords = S.size allwords
                              nummatches = S.size (S.intersection dictwords allwords)
                              proportion = fromIntegral nummatches / fromIntegral numwords
                          in proportion > 0.5


