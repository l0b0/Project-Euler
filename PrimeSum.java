/**
 * $Id$
 *
 * Find the sum of all the primes below N.
 * Solution to http://projecteuler.net/index.php?section=problems&id=10
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

public class PrimeSum {
	
	/**
	 * Indexes in result correspond to integer value minus 2.
	 * I.e., index 0 corresponds to the number 2.
	 * This is done for convenience when looping through all values.
	 * http://projecteuler.net/index.php?section=problems&id=10
	 * @param number Max value for the primes
	 * @return Eratosthenes' sieve of primes
	 */
	public static boolean[] primeSieve(int max) {
		boolean[] sieve = new boolean[max];
		if (max < 2) {
			System.out.println("Max must be >= 2. Returning null result.");
		} else {
			for (int index = 0; index < sieve.length - 1; index++) {
				sieve[index] = true; // Assume that all numbers are primes
			}
			
			// Special case for 2
			int prime = 2;
			for (int index = prime - 1; index < sieve.length - 1; index++) {
				if ((index + 2) % prime == 0) {
					sieve[index] = false;
				}
			}

			prime = 3;
			do {
				//System.out.println("New prime: " + prime);
				/* Set all multiples to false.
				 * Must start index at prime - 1, which is the boolean for the number after the current prime.
				 */
				for (int index = prime - 1; index < sieve.length - 1; index++) {
					if ((index + 2) % prime == 0) {
						sieve[index] = false;
					}
				}

				// Get the next prime (next true value)
				do {
					prime += 2;
				} while (prime <= max && !sieve[prime - 2]);
			} while (prime <= max);
		}
		return sieve;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int max = 1000000;
		//int max = 10;

		// Get all the necessary primes
		boolean[] primes = primeSieve(max); // Primes found along the way
		
		// Sum up all the primes except the last one
		long sum = 0;
		for (int index = 0; index < primes.length - 1; index++) {
			if (primes[index]) sum += index + 2;
		}
		System.out.println("Result: " + sum);
	}
}