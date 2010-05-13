/**
 * $Id$
 *
 * Find the largest prime factor of X
 * Solution to http://projecteuler.net/index.php?section=problems&id=3
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

import java.util.*;

public class LastFactor {
	
	private final long number;
	private final long lastFactor; // Highest factor in number
	private ArrayList<Long> primes = new ArrayList<Long>(1); // Primes found along the way

	/**
	 * @param number The number which should be broken into primes
	 */
	public LastFactor(long number) throws NullPointerException {
		super();
		this.number = number;

		primes.add((long)2); // Initialize
		primes.add((long)3); // To make nextPrime work
		
		long remainder = this.number; // Starting point
		long factor; // Current factor (0 while none has been found)
		ListIterator<Long> primeIterator;
		
		int panicCounter = 0;
		do {
			if (panicCounter++ > 100000) {
				throw new NullPointerException("*Running home to momma*");
			}
			
			factor = 0; // Not found
			primeIterator = primes.listIterator();

			// Look for factors in current primes
			while (factor == 0 && primeIterator.hasNext()) {
				factor = (long)primeIterator.next();
				if (remainder%factor != 0) {
					factor = 0; // Not a factor; reset
				}
				if (factor != 0) {
					System.out.println("Factor for " + remainder + " found in existing primes: " + factor);
				}
			}
			
			// Gotta find more primes until we find a factor
			while (factor == 0) {
				factor = nextPrime(this.primes);
				if (remainder%factor != 0) {
					factor = 0; // Not a factor; reset
				}
				if (factor != 0) {
					System.out.println("Factor for " + remainder + " found in new primes: " + factor);
				}
			}
			
			// Finally, modify the remainder			
			remainder /= factor;
		} while (remainder > 1);
		lastFactor = factor;
	}
	
	/**
	 * @return The first prime number higher than the last one in the "primes" list.
	 */
	public static long nextPrime(ArrayList<Long> primes) {
		long guess = primes.get(primes.size() - 1); // Last element
		boolean isPrime;
		do {
			guess += 2; // Next odd number
			isPrime = true; // To be falsified
			for (long prime : primes) {
				if (guess%prime == 0) {
					isPrime = false;
				}
			}
		} while (!isPrime);

		//System.out.println("New prime: " + guess);
		
		primes.add(guess);
		return guess;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		LastFactor p = new LastFactor(new Long("317584931803"));
		System.out.println("Last prime factor:");
		System.out.println(p.lastFactor);
	}
}
