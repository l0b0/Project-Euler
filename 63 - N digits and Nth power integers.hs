{- $Id$

How many N-digit positive integers exist which are also an Nth power?
Solution to http://projecteuler.net/index.php?section=problems&id=63

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

positiveIntegers :: [Integer]
positiveIntegers = [1..]

nDigitNthPowers :: [Int]
-- base and exponent are in positiveIntegers, and (base ^ exponent) is (exponent) digits long
-- TODO: Find halt rule (base / exponent too big)
nDigitNthPowers = [base ^ exponent | ]

main :: IO ()
main = putStrLn (show (length nDigitNthPowers) )
