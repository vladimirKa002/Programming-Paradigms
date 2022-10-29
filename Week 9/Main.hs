data Iter a
  = Done a
  | Step (Iter a)
  deriving (Show)

-- Task 1
insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x : xs) 
  | a < x     = [a, x] ++ xs
  | otherwise = [x] ++ (insert a xs)

-- Task 2
approximate :: (a -> Bool) -> (a -> a) -> a -> Iter a
approximate check f a
  | (check a) == True = Done a
  | otherwise         = Step (approximate check f (f a))

-- Task 3
-- a)
eval :: Iter a -> a
eval (Done v) = v
eval (Step iter) = eval iter
-- b)
limit :: Int -> Iter a -> Iter (Maybe a)
limit 0 _        = Done Nothing
limit _ (Done v) = Done (Just v)
limit n (Step iter) = Step (limit (n - 1) iter)
-- c)
partialEval :: Int -> Iter a -> Iter a
partialEval 0 v           = v
partialEval _ (Done v)    = Done v
partialEval n (Step iter) = partialEval (n - 1) iter
-- d)
steps :: Iter a -> Int
steps (Done _)    = 0
steps (Step iter) = (steps iter) + 1

-- Task 4
-- a)
mapIter :: (a -> b) -> Iter a -> Iter b
mapIter f (Done res)  = Done (f res)
mapIter f (Step iter) = Step (mapIter f iter)
-- b)
{-clearIter :: Iter (Iter a) -> Iter a
clearIter (Done (Step iter)) = clearIter (Step (Done iter))
clearIter (Done iter) = iter
clearIter (Step step) = Step(clearIter step)

joinIter :: Iter (Iter a) -> Iter a
joinIter (Done iter) = joinIter (joinIter' (Done iter))
joinIter (Step iter) = joinIter iter-}

-- Task 5
insertIter' :: Int -> [Int] -> [Int] -> Iter [Int]
insertIter' a left [] = Done (left ++ [a])
insertIter' a left (x : xs) 
  | a < x     = Step (Done (left ++ [a, x] ++ xs))
  | otherwise = Step (insertIter' a (left ++ [x]) xs)

insertIter :: Int -> [Int] -> Iter [Int]
insertIter a arr = insertIter' a [] arr

-- Task 6
resultExtractor :: Iter a -> a
resultExtractor (Done a) = a
resultExtractor (Step iter) = resultExtractor iter

coverSteps :: Int -> Iter a -> Iter a
coverSteps 0 val = val
coverSteps n val = Step(coverSteps (n - 1) val)

insertionSortIter' :: [Int] -> [Int] -> Iter [Int]
insertionSortIter' left [] = Done left
insertionSortIter' left (x : xs) = do
  let inserted = insertIter x left
  let res = resultExtractor inserted
  let stps = steps inserted
  coverSteps stps (insertionSortIter' res xs)

inverse :: [Int] -> [Int]
inverse [] = []
inverse (x : xs) = (inverse xs) ++ [x]

insertionSortIter :: [Int] -> Iter [Int]
insertionSortIter arr = insertionSortIter' [] (inverse arr)

main :: IO ()
main = do
  {-print(insert 3 [1,2,5,7])
  print(insert 3 [0,1,1])
  print(take 5 (insert 3 [1..]))-}
  {-print(approximate (\x -> x^2 < 1) (/ 2) 3)
  print(approximate (\x -> x^2 < 0.01) (/ 2) 3)-}
  -- print(eval (approximate (\x -> x^2 < 0.01) (/ 2) 10))
  {-print(limit 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3))
  print(limit 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3))
  print(limit 0 (approximate (\x -> x^2 < 0.01) (/ 2) 3))-}
  {-print(partialEval 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3))
  print(partialEval 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3))-}
  -- print(steps (approximate (\x -> x^2 < 0.01) (/ 2) 3))
  {-print(mapIter (+1) (Done 3))
  print(mapIter (+1) (Step (Step (Done 3))))-}
  
  -- print(joinIter (Step (Done (Done (Done (Step (Done 3)))))))

  {-print(insertIter 1 [2, 3])
  print(insertIter 4 [2, 3])-}
  print(insertionSortIter [1..4])
  print(insertionSortIter [4,3..1])
  print(steps (insertionSortIter [1..10]))
  print(steps (insertionSortIter [10,9..1]))
  