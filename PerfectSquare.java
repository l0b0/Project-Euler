/**
 * $Id$
 *
 * Find the smallest x + y + z with integers x > y > z > 0 such that
 * x + y, x - y, x + z, x - z, y + z, y - z are all perfect squares.
 * Solution to http://projecteuler.net/index.php?section=problems&id=142
 *
 * Copyright 2007 Victor Engmark
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * @author vengmark
 */
package net.projecteuler;

public class PerfectSquare {
	
	public static boolean isPerfectSquare(long number) {
		// Mathematical trick - Check if the floor of the square root squared is the same as the original number
		long sqrt = (long)Math.sqrt(number);
		return (sqrt * sqrt == (double)number);
	}

	/**
	 * x + y is a perfect square
	 * x - y is a perfect square
	 * x + z is a perfect square
	 * x - z is a perfect square
	 * y + z is a perfect square
	 * y - z is a perfect square
	 * @param args
	 */
	public static void main(String[] args) {
		/* Incremental approach: 
		 * Find x and y so that x + y and x - y are perfect squares
		 * Then increment z until y and see if the other constraints are met.
		 */
		long z = 1;
		long y = 2;
		long x = 3;
		
		boolean result = false;
		while (!result) {
			if (!isPerfectSquare(x + y) || !isPerfectSquare(x - y)) { // No match yet, gotta increment x or y
				//System.out.println("0/3 with x " + x + ", y " + y + ", z " + z);
				if (x == y + 1) { // Can't increment y
					x++;
				} else {
					y++;
				}
			} else if (!isPerfectSquare(x + z) || !isPerfectSquare(x - z)) { // No match yet, gotta increment x or z
				//System.out.println("1/3 with x " + x + ", y " + y + ", z " + z);
				if (y == z + 1) { // Can't increment z
					x++;
				} else {
					z++;
				}
			} else if (!isPerfectSquare(y + z) || !isPerfectSquare(y - z)) { // No match yet, gotta increment y or z
				System.out.println("2/3 with x " + x + ", y " + y + ", z " + z);
				if (x == y + 1 && y == z + 1) { // Can't increment y or z
					x++;
				} else if (y == z + 1) { // Can't increment z
					y++;
				} else {
					z++;
				}
			} else {
				result = true;
				System.out.println("Finished with x " + x + ", y " + y + ", z " + z);
			}
		}
	}
}
