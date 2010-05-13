/**
 * $Id$
 *
 * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x,
 * and 6x contain the same digits in some order.
 * Solution to http://projecteuler.net/index.php?section=problems&id=52
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

public class SameDigits {
	
	public static boolean sameDigitsInMultiples(int number, int[] multiples) {
		int[] numbers = new int[multiples.length];
		for (int multipleCount = 0; multipleCount < multiples.length; multipleCount++) {
			//System.out.println(number * multiples[multipleCount]);
			numbers[multipleCount] = number * multiples[multipleCount];
		}
		return sameDigits(numbers);
	}
	
	/**
	 * Assumes that all numbers are the same length.
	 * @param numbers the numbers to check
	 * @return whether all numbers contain the same digits, in some order
	 */
	public static boolean sameDigits(int[] numbers) {		
		char[] firstNumber = Integer.toString(numbers[0]).toCharArray();
		java.util.Arrays.sort(firstNumber); // Important for the next step
		
		char[] number;
		// Start from the second, and check against the first
		for (int numberCount = 1; numberCount < numbers.length; numberCount++) {
			number = Integer.toString(numbers[numberCount]).toCharArray();
			java.util.Arrays.sort(number);
			if (!java.util.Arrays.equals(firstNumber, number)) return false;
		}
		return true;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int number = 0;
		int[] multiples = new int[]{1,2,3,4,5,6};
		Date startTime = new Date();
		do {
			/* Shouldn't test numbers whose first digit is > 1; they will have a different
			 * digit count when multiplied by >= 5.
			 */
			if (Reversibles.reverseDigits(number)%10 == 1) {
				number++;
			} else {
				number = (int)Math.pow(10, Integer.toString(number).length()); // Step up to next level
			}
		} while (!sameDigitsInMultiples(number, multiples));
		System.out.println("Reversible when multiplied by 1 through 6: " + number);
		System.out.println("Total time used (ms): " + (new Date().getTime() - startTime.getTime()));
	}
}
