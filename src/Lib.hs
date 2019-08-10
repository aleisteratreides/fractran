module Lib
    ( someFunc,
      calculateSequence,
      calculateFinalNumber,
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
calculateSequence n programm = 
  if isNothing next
    then []
    else next : calculateSequence (fromJust next) programm
    where next = nextNumber n programm

calculateFinalNumber :: Integer -> Program -> Maybe Integer
calculateFinalNumber n program = go (Just n) program Nothing
  where 
    go Nothing _ last = last
    go n program _ = go (nextNumber (fromJust n) program) program n

-- find 3^(12+4)
someFunc :: IO ()
someFunc = print $ calculateFinalNumber (2^12 * 3^4) [3 % 2]
