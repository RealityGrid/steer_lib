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
 * This abstract class provides a superclass for the internal SWIG generated
 * classes such as SWIGTYPE_p_int and so on.
 *
 * @version 1.2b
 * @author Robert Haines
 */
public abstract class ReG_SWIG {

  /**
   * Compares two objects of type ReG_SWIG to see if they are the same object
   * (their pointers are the same).
   *
   * @return true if the two objects are the same object, false otherwise.
   */
  public boolean equals(Object obj) {
    boolean equal = false;
    if (obj instanceof ReG_SWIG)
      equal = (((ReG_SWIG)obj).getPointer() == this.getPointer());
    return equal;
  }
  
  /**
   * Provides a numerical representation of the internal SWIG pointer
   * to this object. Used in comparison routines and so on.
   *
   * @return the pointer to this object.
   * @see #equals(Object)
   */
  protected abstract long getPointer();

  /**
   * Provides a C/C++ style void pointer to this object.
   *
   * @return the void pointer to this object.
   */
  public SWIGTYPE_p_void getVoidPointer() {
    return new SWIGTYPE_p_void(getPointer(), false);
  }
}
