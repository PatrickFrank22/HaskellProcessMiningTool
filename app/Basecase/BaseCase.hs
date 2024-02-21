module Basecase.BaseCase (
    singleActivity,
    singleActivityFiltering
) where

{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

import Helperfunctions.HelperFunctions


singleActivity :: [(Int, [String])] -> [String]
singleActivity [(_, [x])] = [x]
singleActivity _ = []



singleActivityFiltering :: Float -> [(Int, [String])] -> [String]
singleActivityFiltering f log = 
  let x = allActivities log
  in if length x == 1 && x /= [tau]
     then if abs ((intToFloat (numberOfTraces log)) / (intToFloat (numberOfEvents log + numberOfTraces log)) - 0.5) <= f
          then x
          else []
     else []
       where
         numberOfTraces :: [(Int, [String])] -> Int
         numberOfTraces [] = 0
         numberOfTraces ((occ,_):xs) = occ + numberOfTraces xs

         numberOfEvents :: [(Int, [String])] -> Int
         numberOfEvents [] = 0
         numberOfEvents ((_,[tau]):xs) = numberOfEvents xs
         numberOfEvents ((occ,x):xs) = occ * (length x) + numberOfEvents xs
