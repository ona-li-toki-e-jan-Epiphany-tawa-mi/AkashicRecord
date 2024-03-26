import Control.Concurrent

-- lil fizzbuzz boio
fizzBuzzify :: Int -> [Char]
fizzBuzzify n 
  | mod3 == 0 && mod5 == 0 = "fizzbuzz"
  | mod3 == 0              = "fizz"
  | mod5 == 0              = "buzz"
  | otherwise              = show n 
  where mod3 = n `mod` 3
        mod5 = n `mod` 5

main :: IO ()
main = loopFizzbuzz 0
  where
    loopFizzbuzz :: Int -> IO () 
    loopFizzbuzz n = do
      print $ fizzBuzzify n
      threadDelay (500 * 1000)
      loopFizzbuzz (n + 1)
