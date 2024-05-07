#!/usr/bin/env runhaskell

{- MIT License

  Copyright (c) 2023 ona-li-toki-e-jan-Epiphany-tawa-mi

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE. -}

import System.Process
import Control.Monad



-- |A code used to unlock the bootloader of an Android phone.
type UnlockCode = String

unlockCodeLength :: Int
unlockCodeLength = 16

alphanumerics :: String
alphanumerics = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

-- |A list of all the possible unlock codes.
unlockCodes :: [UnlockCode]
unlockCodes = replicateM unlockCodeLength alphanumerics



bruteForce :: [UnlockCode] -> IO ()
bruteForce (code:rest) = do
  putStrLn $ "Trying code '" ++ code ++ "'..."
  callCommand $ "fastboot oem unlock " ++ code

  bruteForce rest



-- |Attempts to bruteforce the oem unlock code to unlock the bootloader of an
-- Android phone. The phone will need to have oem unlock and USB debugging
-- enabled in the settings menu, it will need to be hooked up to the computer
-- via adb, and it must be running in fastboot mode. Note that this may not work
-- on all phones, as they have a limit on the number of attempts that can be
-- made, or, at the very least, this may take an EXTREMLY long time.
main :: IO ()
main = bruteForce unlockCodes
