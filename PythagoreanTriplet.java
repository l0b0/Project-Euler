/**
 * $Id$
 *
 * A Pythagorean triplet is a set of three natural numbers, a<b<c, for which a^2 + b^2 = c^2
 * Solution to http://projecteuler.net/index.php?section=problems&id=9
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

public class PythagoreanTriplet {
	
	public static boolean isPythagoreanTriplet(final long a, final long b, final long c) {
		return (a < b && b < c && a * a + b * b == c * c);
	}

	/**
	 * a + b + c == 1000
	 * a < b
	 * b < c
	 * a * a + b * b == c * c
	 * =>
	 * a < 998
	 * b < 999
	 * c < 1000
	 * 
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		int sum = 1000;
		for (int c = 999; c > 3; c--) {
			for (int b = 2; b < c; b++) {
				for (int a = 1; a < b; a++) {
					if (a + b + c == sum) {
						if (isPythagoreanTriplet(a, b, c)) {
							System.out.println("Result: " + a * b * c);
						}
					}
				}
			}
		}
	}

}
