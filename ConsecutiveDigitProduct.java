/**
 * $Id$
 *
 * Find the greatest product of five consecutive digits in the N-digit
 * number.
 * Solution to http://projecteuler.net/index.php?section=problems&id=8
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

import java.math.BigInteger;

public class ConsecutiveDigitProduct {
	private final BigInteger number;
	private final int numberCount;
	private final long largestDigitSequenceProduct;

	/**
	 * @param number
	 * @param numberCount
	 */
	public ConsecutiveDigitProduct(final BigInteger number, final int numberCount) {
		super();
		this.number = number;
		this.numberCount = numberCount;
		this.largestDigitSequenceProduct = digitProduct(this.number, this.numberCount);
	}
	
	public static long digitProduct(BigInteger number, int numberCount) {
		long product = 1;
		long biggestProduct = 0;
		char[] digits = number.toString().toCharArray();
		// Add the first numberCount digits to sum (easier for the next for loop)
		for (int digitCount = 0; digitCount < numberCount; digitCount++) {
			//System.out.println("Adding " + digits[digitCount]);
			product *= Character.getNumericValue(digits[digitCount]);
		}

		for (int digitCount = numberCount; digitCount < digits.length; digitCount++) {
			// If the current sum is the largest, then replace biggestSum
			if (product > biggestProduct) {
				//System.out.println("New biggest product: " + product);
				biggestProduct = product;
			}
			/* Go one step ahead by generally removing the first digit and adding the next one.
			 * If the digit which is removed is zero, information has been lost, and the product
			 * must be rebuilt to stay non-zero
			 */
			if (Character.getNumericValue(digits[digitCount - numberCount]) == 0) {
				// Lost information by multiplying by zero, gotta rebuild product
				product = 1;
				for (int restoreCount = 0; restoreCount < numberCount; restoreCount++) {
					product *= Character.getNumericValue(digits[digitCount - restoreCount]);
				}
				//System.out.println("Restored product: " + product);
			} else {
				product /= Character.getNumericValue(digits[digitCount - numberCount]);
				product *= Character.getNumericValue(digits[digitCount]);
			}
		}
		return biggestProduct;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		BigInteger number = new BigInteger("7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450");
		int numberCount = 5;
		ConsecutiveDigitProduct c = new ConsecutiveDigitProduct(number, numberCount);
		System.out.println("Result: " + c.largestDigitSequenceProduct);
	}
}