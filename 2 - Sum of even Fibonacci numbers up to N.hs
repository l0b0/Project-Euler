{- $Id$

Find the sum of all even-valued Fibonacci numbers until N.
Solution to http://projecteuler.net/index.php?section=problems&id=2

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

import System( getArgs, getProgName )

fibs :: Num num => [num]
fibs = 0 : 1 : [ last + next_last | (last, next_last) <- zip fibs (tail fibs) ]

evens :: (Integral integral) => [integral] -> [integral]
evens list = filter even list

nthFibonacci :: (Num num) => Int -> num
nthFibonacci number = fibs !! number

fibonaccisUntil :: (Num numord, Ord numord) => numord -> [numord]
fibonaccisUntil number = takeWhile (<= number) fibs

evenFibonaccisUntil :: (Integral integral) => integral -> [integral]
evenFibonaccisUntil number = takeWhile (<= number) (evens fibs)

sumEvenFibonaccisUntil :: (Integral integral) => integral -> integral
sumEvenFibonaccisUntil number = sum (evenFibonaccisUntil number)

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
                integers -> putStr . unlines . map (show . sumEvenFibonaccisUntil . read) $ integers