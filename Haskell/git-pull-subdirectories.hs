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
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Data.Functor



-- |Runs git pull on the subdirectories in the directories supplied via command
-- line arguments.
main :: IO ()
main = getArgs >>= parseArguments

parseArguments :: [String] -> IO ()
parseArguments []          = die usageMessage
parseArguments arguments
  | "-h" `elem` arguments = putStrLn usageMessage
  | "-v" `elem` arguments = putStrLn versionMessage
  | otherwise             = do
      success <- mapM gitPullSubdirectories arguments <&> and
      unless success exitFailure

usageMessage :: String
usageMessage = "Usage: git-pull-subdirectories [-hv] DIRECTORY...\n \
               \\n\
               \Runs git pull on the subdirectories in the supplied directories."
versionMessage :: String
versionMessage = "git-pull-subdirectories v0.2.1"



-- |Runs git pull on the subdirectories of the given directory and returns
-- whether all calls to git pull succeeded. Logs to stdout.
gitPullSubdirectories :: FilePath -> IO Bool
gitPullSubdirectories directory = do
  exists        <- doesDirectoryExist directory
  fullDirectory <- makeAbsolute directory
  putStrLn $ "Running git pull on the subdirectories of '" ++ show fullDirectory ++ "'..."

  if exists
    then do
      success <- listDirectory directory <&> map (combine directory)
             >>= filterM doesDirectoryExist
             >>= mapM gitPullDirectoryUnsafe
             <&> and
      unless success $ stderrPutStrLn $ "Failed running git pull on subdirectories of '" ++ show fullDirectory ++ "': git pull command failed! See previous log messages for details"
      return success

    else do
      stderrPutStrLn $ "Failed running git pull on the subdirectories of '" ++ show fullDirectory ++ "': the directory is not a directory or does not exist!"
      return False

-- |Navigates into a directory, runs git pull, navigates back, and returns
-- whether git pull succeeded. Logs to stdout. Marked as unsafe because this
-- function will not check if the given directory exists.
gitPullDirectoryUnsafe :: FilePath -> IO Bool
gitPullDirectoryUnsafe directory = do
  fullDirectory <- makeAbsolute directory
  putStrLn $ "Running git pull on '" ++ show fullDirectory ++ "'..."

  exitCode <- spawnCommand ("git -C '" ++ directory ++ "' pull") >>= waitForProcess

  case exitCode of
    ExitSuccess             -> return True
    (ExitFailure errorCode) -> do
      stderrPutStrLn $ "ERROR: Failed running git pull on '" ++ show fullDirectory ++ "': git pull failed with error code: " ++ show errorCode ++ "!"
      return False



-- |Prints a string to stderr with an ending newline.
stderrPutStrLn :: String -> IO ()
stderrPutStrLn = hPutStrLn stderr
