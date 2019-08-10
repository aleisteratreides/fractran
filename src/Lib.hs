module Lib
    ( calculateSequence,
      calculateFinalNumber,
      sumProgram,
      subtractionProgram,
    ) where


import GHC.Real
import Data.Maybe

type Program = [Ratio Integer]

nextNumber :: Integer -> Program -> Maybe Integer
nextNumber _ [] = Nothing
nextNumber n (x:xs) =
  if denominator multiplied == 1 
    then Just (numerator multiplied)
    else nextNumber n xs
    where multiplied = (x * fromIntegral n)

calculateSequence :: Integer -> Program -> [Maybe Integer]
calculateSequence n program = 
  if isNothing next
    then []
    else next : calculateSequence (fromJust next) program
    where next = nextNumber n program

calculateFinalNumber :: Integer -> Program -> Maybe Integer
calculateFinalNumber n program = go (Just n) program Nothing
  where 
    go Nothing _ last = last
    go n program _ = go (nextNumber (fromJust n) program) program n


sumProgram :: Program
sumProgram = [3 % 2]

subtractionProgram :: Program
subtractionProgram = [1 % 6]
