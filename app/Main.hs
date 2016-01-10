{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc.Lordown (convert)

main :: IO ()
main = interact convert
