/**
 * $Id$
 *
 * Find the sum of all the even-valued terms in the Fibonacci sequence
 * which do not exceed one million.
 * Solution to http://projecteuler.net/index.php?section=problems&id=2
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

public class FibonacciSequence {

	private final long end; // Maximum value of the last number
	private Vector<Long> sequence = new Vector<Long>(2, 1); // Two numbers initially, increase by 1.

	/**
	 * @param end upper limit of the last value in the sequence
	 */
	public FibonacciSequence(final int end) {
		super();
		this.end = end;
		sequence.clear();
		sequence.add((long)0);
		sequence.add((long)1);
		if (!CreateSequence(end)) {
			System.out.println("Abort!");
		}
	}
		
	/**
	 * @param end upper limit of the last value in the sequence
	 */
	public FibonacciSequence(final int end, final int start1, final int start2) {
		super();
		this.end = end;
		sequence.clear();
		sequence.add((long)start1);
		sequence.add((long)start2);
		if (!CreateSequence(end)) {
			System.out.println("Abort!");
		}
	}
		
	public boolean CreateSequence(int end) {
		if (end < 1) {
			return false;
		}
		Long next = null;
		while ((next = sequence.elementAt(sequence.size() - 2) + sequence.lastElement()) < this.end) {
			if (!sequence.add(next)) {
				System.out.println("Couldn't add Fibonacci number after " + sequence.lastElement());
				return false;
			}
		}
		return true;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		FibonacciSequence f = new FibonacciSequence(1000000);
		System.out.println("Full sequence:");
		for (Long fnum : f.sequence) {
			System.out.println(fnum);
		}
		System.out.println("\nEven numbers:");
		long sumEven = 0;
		for (Long fnum : f.sequence) {
			if (fnum%2 == 0) {
				sumEven += fnum;
				System.out.println(fnum);
			}
		}
		System.out.println("\nSum of even numbers: " + sumEven);
	}
}