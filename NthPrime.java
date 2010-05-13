/**
 * $Id$
 *
 * What is the Nth prime number?
 * Solution to http://projecteuler.net/index.php?section=problems&id=7
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

public class NthPrime {
	
	public static long nthPrime(ArrayList<Long> primes, int n) {
		while (primes.size() < n) {
			LastFactor.nextPrime(primes);
		}
		return primes.get(primes.size() - 1);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Date startTime = new Date();
		int n = 10001;
		ArrayList<Long> primes = new ArrayList<Long>(1); // Primes found along the way
		primes.add((long)2);
		primes.add((long)3);
		System.out.println(n + "th prime: " + nthPrime(primes, n));
		System.out.println("Total time used (ms): " + (new Date().getTime() - startTime.getTime()));
	}
}