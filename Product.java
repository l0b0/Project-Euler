/**
 * $Id$
 *
 * Find the largest palindrome made from the product of two 3-digit
 * numbers.
 * Solution to http://projecteuler.net/index.php?section=problems&id=4
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

import net.projecteuler.Reversibles;

public class Product {
	
	private final int product;

	/**
	 * @param product
	 */
	public Product(final int factor1, final int factor2) {
		super();
		this.product = factor1 * factor2;
	}
	
	public static boolean isPalindrome(int number) {
		return number == Reversibles.reverseDigits(number);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// Look for biggest palindrome product down from 999
		int biggestProduct = 0;
		int biggestFactor1 = 0;
		int biggestFactor2 = 0;
		for (int factor1 = 999; factor1 > 99; factor1--) {
			for (int factor2 = 999; factor2 > 99; factor2--) {
				Product product = new Product(factor1, factor2);
				if (isPalindrome(product.product) && product.product > biggestProduct) {
					biggestProduct = product.product;
					biggestFactor1 = factor1;
					biggestFactor2 = factor2;
				}
			}
		}
		System.out.println("Product of " + biggestFactor1 + " and " + biggestFactor2 + ": " + biggestProduct);
	}
}