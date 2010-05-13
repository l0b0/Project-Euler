{- $Id$

What is the first term in the Fibonacci sequence to contain 1000 digits?
Solution to http://projecteuler.net/index.php?section=problems&id=25

Copyright 2007 Victor Engmark

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

module Main( main ) where

import System ( getArgs, getProgName )

{-
Note that this starts at 1, not 0 like in
http://en.wikipedia.org/wiki/Fibonacci_number
-}
fibs :: Num num => [num]
fibs = 1 : 1 : [ last + next_last | (last, next_last) <- zip fibs (tail fibs) ]

{-
Get the number of digits in a number
-}
digitCount :: Num num => num -> Int
digitCount number = length (show number)

fibonaccisBelowNDigits :: Num num => Int -> [num]
fibonaccisBelowNDigits number = takeWhile (\x -> (digitCount x < number)) fibs

firstFibonacciWithNDigits :: Int -> Int
firstFibonacciWithNDigits number = (length (fibonaccisBelowNDigits number)) + 1

usageAndExit :: IO ()
usageAndExit  = do
        progName <- getProgName
        putStrLn $ "Usage: runghc \"" ++ progName ++ "\" [integers]"
        putStrLn $ "where [integers] is a space separated list."

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> usageAndExit
                integers -> putStr . unlines . map (show . firstFibonacciWithNDigits . read) $ integers
