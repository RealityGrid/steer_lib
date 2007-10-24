/* ----------------------------------------------------------------------------
  This file is part of the RealityGrid Steering Library Java Wrappers.
 
  (C) Copyright 2006, University of Manchester, United Kingdom,
  all rights reserved.
 
  This software was developed by the RealityGrid project
  (http://www.realitygrid.org), funded by the EPSRC under grants
  GR/R67699/01 and GR/R67699/02.
 
  LICENCE TERMS
 
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
 
  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
  CORRECTION.
 
  Author........: Robert Haines
---------------------------------------------------------------------------- */

import org.realitygrid.utilities.IO;

public class IOExample {

  public static void main(String[] argv) {
    System.out.print("Enter a character: ");
    char testChar = IO.readChar();
    System.out.println("You entered: " + testChar);

    System.out.print("\nEnter an integer: ");
    int testInt = IO.readInt();
    System.out.println("You entered: " + testInt);

    System.out.print("\nEnter a float: ");
    float testFloat = IO.readFloat();
    System.out.println("You entered: " + testFloat);

    System.out.print("\nEnter a double: ");
    double testDouble = IO.readDouble();
    System.out.println("You entered: " + testDouble);

    System.out.print("\nEnter a line of text: ");
    String testLine = IO.readLine();
    System.out.println("You entered: " + testLine);

    System.out.print("\nEnter free text: ");
    String test = IO.readText();
    System.out.println("\nYou entered:");
    for(int i = 0; i < test.length(); i++) {
      if(test.charAt(i) == '\n') System.out.println("\\n");
      else System.out.print(test.charAt(i));
    }
  }

}
