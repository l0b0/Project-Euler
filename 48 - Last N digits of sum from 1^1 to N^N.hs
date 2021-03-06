{- $Id$

Find the last ten digits of 1^1 + 2^2 + ... + 1000^1000.
Solution to http://projecteuler.net/index.php?section=problems&id=48

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

NpowerNwithXDigits :: Int -> Int -> Int
NpowerNwithXDigits number digits = 

usageAndExit :: IO ()
usageAndExit  = do
        progName <- getProgName
        putStrLn $ "Usage: runghc " ++ progName ++ " [digit count] [last factor]"

main :: IO ()
main = do
        args <- getArgs
        case args of
                [integer] -> print $ firstFibonacciWithNDigits $ read integer
                _ -> usageAndExit
