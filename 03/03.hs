import Data.List (filter, permutations, transpose)

couldBeTriangle :: [Int] -> Bool
couldBeTriangle a = all couldBeTriangle' (permutations a)
  where
    couldBeTriangle' [a,b,c] = a + b > c
    couldBeTriangle' _ = False

chop :: Int -> [a] -> [[a]]
chop n xs | length xs < n = [xs]
          | otherwise     = take n xs : chop n (drop n xs)

main = do
  input <- getContents
  let vals = map (map read . words) $ lines input :: [[Int]]
  let vals2 = chop 3 $ concat $ transpose vals
  print $ length $ filter couldBeTriangle vals
  print $ length $ filter couldBeTriangle vals2
