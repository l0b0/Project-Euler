/**
 * $Id$
 *
 * Get the sum of the base-10 digits of 2^power
 * Solution to http://projecteuler.net/index.php?section=problems&id=16
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

import java.util.Date;
import java.math.BigInteger;

public class BinaryDigitSum {
	
	private final int power;
	private final long sum;

	/**
	 * @param number
	 * @param sum
	 */
	public BinaryDigitSum(final int power) {
		super();
		this.power = power;
		this.sum = sumDigits(this.power);
	}
	
	public static long sumDigits(int power) {
		BigInteger product = new BigInteger("2").pow(power);
		long sum = 0;
		BigInteger divisor = new BigInteger("10");
		do {
			sum += product.mod(divisor).intValue();
			product = product.divide(divisor);
		} while (product.compareTo(new BigInteger("1")) >= 0); // Euphemism for (product >= 1)
		return sum;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Date startTime = new Date();
		BinaryDigitSum s = new BinaryDigitSum(1000);
		System.out.println("Sum of base-10 digits in 2^" + s.power + " = " + s.sum);
		System.out.println("Total time used (ms): " + (new Date().getTime() - startTime.getTime()));
	}
}