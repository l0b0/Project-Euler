{- $Id$

Find the sum of the digits in the number 100!
Solution to http://projecteuler.net/index.php?section=problems&id=20

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

factorial :: Int -> Integer
factorial number = map (* [1..number]) 

digitSum :: Integer -> Integer
digitSum number = 1

usageAndExit :: IO ()
usageAndExit  = do
        progName <- getProgName
        putStrLn $ "Usage: runghc " ++ progName ++ " [positive integer]"

main :: IO ()
main = do
        args <- getArgs
        case args of
                [integer] -> print $ digitSum $ factorial $ read integer
                _ -> usageAndExit
