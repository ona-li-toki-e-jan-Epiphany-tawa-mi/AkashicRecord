-- Fibbernachi sequence.
fibbonachi :: Int -> Int
-- TODO Make faster
fibbonachi n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibbonachi (n - 1) + fibbonachi (n - 2)