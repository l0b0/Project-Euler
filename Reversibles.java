/**
 * $Id$
 *
 * How many reversible numbers are there below X?
 * Solution to http://projecteuler.net/index.php?section=problems&id=145
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

public class Reversibles {
	
	private long max;
	private ArrayList<Integer> reversibles = new ArrayList<Integer>(1); // Reversible numbers found along the way
	
	// Debug variables
	int constructorTime = 0;
	int isReversibleCounter = 0;
	long isReversibleTime = 0;
	int reverseDigitsCounter = 0;
	long reverseDigitsTime = 0;
	int addCounter = 0;
	long addTime = 0;

	/**
	 * @param reversibles
	 */
	public Reversibles(long max) {
		super();
		Date startTime = new Date();
		this.max = max;
		for (int counter = 1; counter <= max; counter += 1) {
			if (isReversible(counter)) {
				//Date counterTime = new Date();
				reversibles.add(counter);
				//addTime += (new Date().getTime() - counterTime.getTime());
				//addCounter++;
			}
		}
		constructorTime += (new Date().getTime() - startTime.getTime());
	}
	
	public boolean isReversible(int number) {
		//isReversibleCounter++;
		//Date startTime = new Date();
		int reverseNumber = reverseDigits(number);
		//System.out.println("Number " + number + ", reversed " + reverseNumber);
		//String numberString = Long.toString(number);
		
		// First or last digit can't be zero (don't need to check first digit of number)
		if (number%10 == 0) {
			//isReversibleTime += (new Date().getTime() - startTime.getTime());
			return false;
		}
		
		// Check that all digits in sum are odd
		for (char digit : Long.toString(number + reverseNumber).toCharArray()) {
			if (digit%2 == 0) {
				//isReversibleTime += (new Date().getTime() - startTime.getTime());
				return false;
			}
		}
		
		// All travails passed
		//isReversibleTime += (new Date().getTime() - startTime.getTime());
		//System.out.println("Adding " + number + " + " + reverseNumber + " = " + (number + reverseNumber));
		return true;
	}
		
	public static int reverseDigits(int number) {
		//Date startTime = new Date();
		int tempnum = number;
		int newnum = 0;
		do {
			newnum = newnum * 10 + tempnum % 10;
			tempnum = tempnum / 10;
		} while (tempnum >= 1);
		//reverseDigitsTime += (new Date().getTime() - startTime.getTime());
		//reverseDigitsCounter++;
		//System.out.println("Number " + number + ", reversed " + newnum);
		return newnum;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int max = 1000000000;
		Reversibles r = new Reversibles(max);
		System.out.println("Reversibles below " + r.max + ": " + r.reversibles.size());
		System.out.println("\nTime used: ");
		System.out.println("isReversible (" + r.isReversibleCounter + " times): " + r.isReversibleTime);
		System.out.println("reverseDigits (" + r.reverseDigitsCounter + " times): " + r.reverseDigitsTime);
		System.out.println("Adding new (" + r.addCounter + " times): " + r.addTime);
		System.out.println("Total time used (ms): " + r.constructorTime);
		
		/*System.out.println("Numbers: ");
		for (long reversible : r.reversibles) {
			System.out.println(reversible);
		}*/
	}

}
