{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc.Lordown (convert)
import qualified Paths_lordown as L
import qualified Data.Version as V
import System.Environment (getArgs)
import System.Exit (die)
import Data.List (intercalate)

usage :: String
usage = "lordown v" ++ version ++ " - Markdown to Lorcode converter\n\
        \Reads Markdown from standard input and writes converted markup to standard output\n\n\
        \Usage: lordown\n\
        \\n\
        \Options:\n\
        \  -h  --help     Show this message\n\
        \  -v  --version  Display version\n"
  
version :: String
version = V.showVersion L.version

abort :: [String] -> IO ()
abort args = die $ "illegal invokation: lordown " ++ intercalate " " args ++ "\n" ++ usage

showUsage :: IO ()
showUsage = putStr usage
                   
showVersion :: IO ()
showVersion = putStrLn version                                               

main :: IO ()
main = do args <- getArgs
          case args of
            ["-h"] -> showUsage
            ["--help"] -> showUsage
            ["-v"] -> showVersion
            ["--version"] -> showVersion
            [] -> interact convert
            _ -> abort args

