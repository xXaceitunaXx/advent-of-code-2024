import Data.List (group, nub, sort)
import Text.Parsec (parse)

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

ocurrences :: Int -> [Int] -> Int
ocurrences numero lista = length $ filter (== numero) lista

allCounts :: [Int] -> [Int] -> [(Int, Int)]
allCounts from list = [(n, ocurrences n list) | n <- from]

main :: IO ()
main = do
  input <- readFile "input1.txt"
  print $ sum $ differences $ parseNumbers input

  print $ sum $ map (uncurry (*)) $ allCounts (evens $ parseNumbers input) (odds $ parseNumbers input)