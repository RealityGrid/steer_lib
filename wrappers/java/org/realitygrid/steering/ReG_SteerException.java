/* ----------------------------------------------------------------------------
  This file is part of the RealityGrid Steering Library Java Wrappers.
 
  (C) Copyright 2005, University of Manchester, United Kingdom,
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

package org.realitygrid.steering;

/**
 * This class provides exceptions for the RealityGrid Steering API. It extends
 * the standard Exception class with an error code that corresponds to the
 * values returned by RealityGrid methods, eg: REG_FAILURE.
 *
 * @version 1.2b
 * @author Robert Haines
 */
public class ReG_SteerException extends RuntimeException {

  private int errorCode;

  /**
   * Constructs a RealityGrid exception with no error code or
   * text description.
   */
  public ReG_SteerException() {
    super();
  }

  /**
   * Constructs a RealityGrid exception with a text description but no
   * error code.
   *
   * @param s The text description.
   */
  public ReG_SteerException(String s) {
    super(s);
  }

  /**
   * Constructs a RealityGrid exception with an error code but no
   * text description.
   *
   * @param ec The error code.
   */
  public ReG_SteerException(int ec) {
    super();
    errorCode = ec;
  }

  /**
   * Constructs a RealityGrid exception with an error code and a
   * text description.
   *
   * @param s The text description.
   * @param ec The error code.
   */
  public ReG_SteerException(String s, int ec) {
    super(s);
    errorCode = ec;
  }

  /**
   * Return the value of the error code of this exception.
   *
   * @return the error code contained in this exception.
   */
  public int getErrorCode() {
    return errorCode;
  }

  /**
   *
   */
  public String toString() {
    String errorMessage = super.toString();
    errorMessage += " ReG Error code: ";
    errorMessage += ReG_SteerUtilities.errorLookup(errorCode);
    return errorMessage;
  }

}
