{- $Id$

Rational zeros of a function of three variables.
Solution to http://projecteuler.net/index.php?section=problems&id=180

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

f1 :: Int -> Int -> Int -> Int -> Int
f1 n x y z = x ** (n + 1) + y ** (n + 1) - z ** (n + 1)
f2 :: Int -> Int -> Int -> Int -> Int
f2 n x y z = (x * y + y * z + z * x) * (x ** (n - 1) + y ** (n - 1) - z ** (n - 1))
f3 :: Int -> Int -> Int -> Int -> Int
f3 n x y z = x * y * z * (x ** (n - 2) + y ** (n - 2) - z ** (n - 2))

fn :: Int -> Int -> Int -> Int -> Int
fn n x y z = f1 n x y z + f2 n x y z + f3 n x y z

goldenTripleOrder :: Int -> Int -> Int -> Int
goldenTripleOrder x y z = 1

s :: Int -> Int -> Int -> Int
s x y z = x + y + z

t :: Int -> Int -> Int
t u v = u / v

sumUVForOrder :: Int -> Int
sumUVForOrder order = 1

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
                integers -> putStr . unlines . map (show . sum . read) $ integers