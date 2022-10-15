{-
Task 1.

guess p g = do
  s <- getLine
  x <- g s
  case p x of
    True -> return x
    False -> guess p g

guess function takes 2 parameters p and g.
Firstly, it gets line as an input and puts it as an arguments for g function and assigns result to x variable.
Then, it gets result of execution of p function with x argument. The result is supposed to be a bool value.
If it is False, function will proceed recursion. Otherwise, it will return x which is a program (IO) that returns variable of type x.
So, it should look like this:

guess :: IO x

We can also notice that x <- g s, so
g :: String -> IO x
-}

-- Task 2.
import Data.Char (toUpper)

strToUpper :: [Char] -> [Char]
strToUpper [] = []
strToUpper (x : xs) =
  [toUpper x] ++ strToUpper (xs)

echo :: IO ()
echo = do
  inp <- getLine
  case inp of
    "exit" -> return ()
    _ -> do
      print (strToUpper inp)
      echo
      return ()

-- Task 3.

-- 3. a)
foreverIO :: IO a -> IO b
foreverIO f = do
  f
  foreverIO f

-- 3. b)
whenIO :: Bool -> IO () -> IO ()
whenIO cond f
  | cond == True  = f
  | otherwise     = return ()

-- 3. c)
maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO mf =
  case mf of
    Nothing -> return Nothing
    Just n  -> do
      res <- n
      return (Just res)

-- 3. d)
sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO [] = return []
sequenceMaybeIO (mf : mfs) = do
  res <- mf
  case res of
    Nothing -> sequenceMaybeIO mfs
    Just r  -> do
      resList <- (sequenceMaybeIO mfs)
      return ([r] ++ resList)

-- 3. e)
whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO f a = do
  res <- f a
  case res of 
    Nothing -> return ()
    Just r  -> whileJustIO f r

testFunction :: Int -> IO (Maybe Int)
testFunction k
  | k > 250   = return Nothing
  | otherwise = do
    print(k)
    testFunction (k * 2)

-- 3. f)
forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s
forStateIO_ s [] f = return s
forStateIO_ s (a : as) f = do
  _s <- (f a s)
  forStateIO_ _s as f

verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
  putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
  return (x:xs)

-- Task 4.
iforIO' :: [a] -> Int -> (Int -> a -> IO ()) -> IO ()
iforIO' [] k f = return ()
iforIO' (a : as) k f = do
  res <- f k a
  iforIO' as (k + 1) f
  return ()

iforIO_ :: [a] -> (Int -> a -> IO ()) -> IO ()
iforIO_ as f = do
  iforIO' as 0 f
  return ()

example = do
  iforIO_ [1, 2] (\i n ->
    iforIO_ "ab" (\j c ->
      print ((i, j), replicate n c)))

main = do
  -- echo
  -- foreverIO (putStrLn "Hello!")
  -- whenIO (True == True) (putStrLn "Hello!")
  -- maybeIO (Just (putStrLn "Hello!"))
  -- sequenceMaybeIO [(return (Just "Hi!")), (return Nothing), (return (Just "Bye!"))]
  -- whileJustIO testFunction 3
  -- forStateIO_ [] [1, 2, 3] verboseCons
  example
  