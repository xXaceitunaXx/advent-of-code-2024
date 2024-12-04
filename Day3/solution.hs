import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.List.Split

getMultiplications :: String -> [String]
getMultiplications line = getAllTextMatches (line =~ "mul\\(([0-9]+),([0-9]+)\\)" :: AllTextMatches [] String)

extractNumbers :: [String] -> [(Int, Int)]
extractNumbers multiplications = map extractPair
  where
    extractPair mul = let (_, _, _, [a, b]) = mul =~ "mul\\(([0-9]+),([0-9]+)\\)" :: (String, String, String, [String])
                      in (read a, read b)

solve :: String -> Int
solve line = sum $ map (uncurry (*)) $ extractNumbers $ getMultiplications line

evens :: [String] -> [String]
evens [] = []
evens [x] = [x]
evens (x : y : xs) = x : evens xs

extractDoS :: String -> String
extractDoS line = concat $ (\(x:xs) -> xs) $ splitOn "do()" line

transformInput :: String -> String
transformInput input = concat $ head splited : map extractDoS splited
    where splited = splitOn "don't()" input

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
    print $ solve $ transformInput input
    -- print $ sum $ map (uncurry (*)) $ extractNumbers $ getMultiplications $ processInput input