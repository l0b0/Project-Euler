{- $Id$

Which is the first triangle number to have over five hundred divisors?
Solution to http://projecteuler.net/index.php?section=problems&id=12

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

nthTriangleNumber :: Num number => number -> number
nthTriangleNumber 1 = 1
nthTriangleNumber index = index + nthTriangleNumber (index - 1)

triangleNumbers :: [Integer]
triangleNumbers = map nthTriangleNumber [1..]

-- Shortcut by looking only at numbers lower than the square root
{- TODO: Is there some way to speed up calculation beyond this?
E.g., making use of primes? -} 
--divisors :: Integral number => number -> [number]
divisors :: Integer -> [Integer]
divisors number = [divisor | divisor <- [1..floor (sqrt number)], mod number divisor == 0]

--firstTriangleNumberWithNDivisors :: Int -> Integer
--firstTriangleNumberWithNDivisors 1 = 1
--
--main :: IO ()
--main = putStrLn (show (firstTriangleNumberWithNDivisors 501) )
