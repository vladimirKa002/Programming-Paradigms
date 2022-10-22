-- Task 1
-- a)
isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _ = False

-- b)
insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) = case (n < x) of
  True -> [n] ++ [x] ++ xs
  False -> [x] ++ (insert n xs)

-- c)
separateBy :: a -> [a] -> [a]
separateBy _ [x] = [x]
separateBy d (x:xs) = [x] ++ [d] ++ (separateBy d xs)

-- d)
splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot _ [] = ([], [])
splitWhenNot cond (x:xs) = case (cond x) of
  False -> ([], [x] ++ xs)
  True  -> do
    let res = splitWhenNot cond xs
    ([x] ++ (fst res), (snd res))

-- e)
groups' :: (a -> Bool) -> [a] -> [a] -> [[a]]
groups' _ left [] = [left]
groups' cond left (x:xs) = case (cond x) of
  True -> [left] ++ (groups' cond [] xs)
  False -> do
    let _left = left ++ [x]
    groups' cond _left xs

groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy cond lst = groups' cond [] lst

-- f)
repl_item :: Int -> a -> [a]
repl_item 0 _ = []
repl_item n it = [it] ++ (repl_item (n - 1) it)

repl_indexed :: Int -> [a] -> [a]
repl_indexed _ [] = []
repl_indexed n (x:xs) = (repl_item n x) ++ (repl_indexed (n + 1) xs)

replicateWithPos :: [a] -> [a]
replicateWithPos lst = repl_indexed 1 lst


-- Task 2
-- a)
lucas' :: Int -> Int -> [Int]
lucas' 0 0 = [2] ++ (lucas' 0 2)
lucas' 0 2 = [1] ++ (lucas' 2 1)
lucas' a b = [a + b] ++ (lucas' b (a + b))

lucas :: [Int]
lucas = lucas' 0 0

-- b)
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 n = [n] ++ (approximationsOfRoot2 (n + (1 / n) - (n / 2)))


main = do
  print(isSingleton [1..])
  print(take 5 (insert 3 [1..]))
  print(take 5 (separateBy 0 [1..]))
  print(take 10 (snd (splitWhenNot (< 100) [1..])))
  print(take 3 (groupsSeparatedBy (\n -> n `mod` 4 == 0) [1..]))
  print(take 10 (replicateWithPos [1..]))

  print(take 10 lucas)
  print(take 5 (approximationsOfRoot2 1))
  