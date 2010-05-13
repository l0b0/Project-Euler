/**
 * $Id$
 *
 * What is the smallest number that is evenly divisible by all of the
 * numbers from 1 to N?
 * Solution to http://projecteuler.net/index.php?section=problems&id=5
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
import java.util.Date;

import net.projecteuler.LastFactor;

public class Divisible {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Date startTime = new Date();
		int maxDivisor = 20;
		System.out.println("Max divisor: " + maxDivisor);
		
		long result = 1;
		
		// Get all the primes below maxDivisor
		ArrayList<Long> primes = new ArrayList<Long>(1);
		primes.add((long)2);
		primes.add((long)3);
		do {
			LastFactor.nextPrime(primes);
		} while (primes.get(primes.size() - 1) < maxDivisor);
		primes.remove(primes.size() - 1); // The last one is after maxDivisor
		
		// The number we're looking for has to have at least the primes as factors
		for (long factor : primes) {
			result *= factor;
			System.out.println("Prime factor: " + factor);
		}
		
		System.out.println("Starting point: " + result);
		
		int divisor = 0;
		result -= 1; // Avoid missing the possibility that the number is the first possibility
		while (divisor < maxDivisor) {
			divisor = 2;
			// Try next number
			result++;
			// Find the highest divisor
			while (result%divisor == 0) {
				divisor++;
			}
		}
		System.out.println("Result: " + result);
		System.out.println("Total time used (ms): " + (new Date().getTime() - startTime.getTime()));
	}
}
