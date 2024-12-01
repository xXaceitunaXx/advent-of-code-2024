import Data.List

parseNumbers :: String -> [Int]
parseNumbers input = map read (words input)

evens :: [Int] -> [Int]
evens [] = []
evens (x : y : xs) = x : evens xs

odds :: [Int] -> [Int]
odds [] = []
odds (x : y : ys) = y : odds ys

differences :: [Int] -> [Int]
differences numbers = map abs $ zipWith (+) (map negate $ sort $ evens numbers) (sort $ odds numbers)

main :: IO ()
main = do
  input <- readFile "input1.txt"
  print $ sum $ differences $ parseNumbers input