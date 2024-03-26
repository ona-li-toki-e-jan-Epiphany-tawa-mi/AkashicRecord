{-# LANGUAGE NumericUnderscores #-}

{-
    Random stuff.
-}

-- multiplicative series of x^3 from 1 to 1000.
xCubedSeries :: Integer
xCubedSeries = product [x ^ 3 | x <- [1 .. 1000]]

-- It will, infact, let you do this. Try and print it lol.
bigBOy :: Integer
bigBOy = sum [5, 10 ..]

-- Titor sequence, it's so coool111!!1
barrel :: [[Char]]
barrel = cycle (replicate 2 "moe" ++ ["kyn"])

fiveHundred :: Int
fiveHundred = head [x | x <- [1 .. 1_000_000], even x, x `mod` 5 == 0, x < 1000, x > 499]

countryInfo :: [Char] -> [Char]
countryInfo "Britan" = "Eww br'ish"
countryInfo _ = "You cool"