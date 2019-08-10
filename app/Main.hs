module Main where

import Lib

main :: IO ()
main = print $ calculateFinalNumber (2^65*3^60) subtractionProgram
