/**
 * $Id$
 *
 * Find the sum of all progressive perfect squares below N.
 * Solution to http://projecteuler.net/index.php?section=problems&id=141
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

import java.util.ArrayList;

public class ProgressivePerfectSquares {
	
	private long maxValue;
	private ArrayList<Long> pps = new ArrayList<Long>();
	
	/**
	 * @param maxValue
	 * @param pps
	 */
	public ProgressivePerfectSquares(long maxValue) {
		super();
		this.maxValue = maxValue;
		long startTime = System.nanoTime(); // debugging

		// Get progressive perfect squares
		long value;
		long maxSqrt = (long)Math.sqrt(this.maxValue - 1); // Must be /less/ than maxValue
		long divisor;
		long quotient = 0;
		long remainder = 0; // Always less than divisor
		/* General comments:
		 * If any of quotient, divisor, or remainder are zero, the result cannot be a geometric sequence,
		 * except for the trivial case of all being zero.
		 * If any two of quotient, divisor, or remainder are equal, the result cannot be a geometric sequence,
		 * except for the trivial case of all being equal (which is impossible with our rules).
		 * Since we're looking for perfect squares, it makes sense to start with that as the base.
		 * Since 9 is the first progressive number, we start there.
		 */
		for (long sqrtValue = 3; sqrtValue <= maxSqrt; sqrtValue++) {
			//System.out.println("\nSquare root: " + sqrtValue); // Debugging
			// Perfect squares only
			value = sqrtValue * sqrtValue;
			//System.out.println("Value: " + value); // Debugging
			/* The divisor must be mimimum 2, otherwise the remainder is zero.
			 * The divisor must be less that the square root of the value, otherwise the remainder is zero.
			 * Up until 16 billion, the average of divisor / sqrt is 0.68. Therefore, it's fastest to search
			 * from the top down.
			 */
			for (divisor = sqrtValue - 1; divisor > 1; divisor--) {
				//System.out.println("Divisor: " + divisor); // Debugging
				/* The remainder can also be zero if the divisor is a factor in the value.
				 * This must be checked.
				 */
				if ((remainder = value % divisor) != 0) {
					//System.out.println("Remainder: " + remainder); // Debugging
					/* value / divisor == quotient * divisor + remainder
					 * if quotient == remainder, then
					 * value / divisor == (quotient + 1) * divisor
					 * I.e., quotient != remainder unless they are both 0 (which has been ruled out above).
					 * Ergo, we don't have to test quotient == remainder
					 */
					quotient = value / divisor; // Rounded down automatically
					/* Quotient and remainder can also be equal, which would mess up the result as well.
					 * Must be tested.
					 * So far, all positive results have had remainder < divisor < quotient.
					 * If they are a geometric sequence, that must mean that
					 * remainder / divisor == divisor / quotient <=>
					 * remainder == divisor^2 / quotient <=>
					 * remainder * quotient == divisor^2
					 */
					if (quotient != divisor && remainder * quotient == divisor * divisor) {
						System.out.println(value + " = " + quotient + " * " + divisor + " + " + remainder
								+ " (at " + (System.nanoTime() - startTime) / 1000000 + " ms)");
						pps.add(value);
					}
				}
			}
		}
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		long startTime = System.nanoTime();
		long maxValue = new Long("1000000000000");
		//long maxValue = new Long("10405");
		ProgressivePerfectSquares p = new ProgressivePerfectSquares(maxValue);
		long sum = 0;
		for (long pps : p.pps) {
			sum += pps;
		}
		System.out.println("Sum of progressive perfect squares below " + maxValue + ": " + sum);
		System.out.println("Total time used (ms): " + (System.nanoTime() - startTime)/1000000);
	}
}