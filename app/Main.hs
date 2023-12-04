module Main where

import GHC.Real
import Data.Maybe
import Lib

showSummation = calculateFinalNumber (2^num1 * 3^num2) sumProgram
    where num1 = 100
          num2 = 100

showSubstraction = calculateFinalNumber (2^num1 * 3^num2) subtractionProgram
    where num1 = 50
          num2 = 47

showSummationExplicit = calculateSequence (2^num1 * 3^num2) subtractionProgram
    where num1 = 10
          num2 = 7

showPrimeNumbers = calculateSequence 10 primeNumbers

isInt x = x == fromInteger (round x)

logTenMaybe = logBase 10 . fromIntegral . fromJust

isPowerOfTen Nothing = False
isPowerOfTen x = isInt (logTenMaybe x)

main :: IO ()
main = do
    -- print $ showSummation
    -- print $ showSubstraction
    -- print $ showSummationExplicit
    let h = head showPrimeNumbers
    let firstPrimes = take 10 (filter isPowerOfTen (showPrimeNumbers))
    print $ zip firstPrimes (map logTenMaybe firstPrimes)
