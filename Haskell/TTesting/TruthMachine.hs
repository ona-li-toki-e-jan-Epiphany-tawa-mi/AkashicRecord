loopPrint1 :: IO ()
loopPrint1 = do
    putStr "1"
    loopPrint1

-- Woooh wacky truth machine with SIDE EFFECTS woo!
main :: IO ()
main = do
    input <- getLine

    case input of 
        "0" -> putStrLn "0"
        "1" -> loopPrint1
        _   -> do
            putStrLn "Invalid value!"
            main