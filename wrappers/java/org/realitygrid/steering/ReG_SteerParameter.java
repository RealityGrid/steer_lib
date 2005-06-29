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
 * This class provides a steerable parameter. It can be of type REG_INT,
 * REG_FLOAT or REG_DBL. The REG_CHAR data type is not supported in
 * Java at present due to the way in which SWIG treats pointers to char.
 *
 * @version 1.2b
 * @author Robert Haines
 */
public class ReG_SteerParameter implements ReG_SteerConstants {

  private String label;
  private int type;
  private boolean steerable;
  private String minLabel;
  private String maxLabel;

  private Object param;

  /**
   * Constructs a RealityGrid Steered Parameter of the requested type
   * but does not set its value.
   *
   * @param l a descriptive label for the parameter.
   * @param st set the parameter to be steerable (if true) or just
   * monitored (if false).
   * @param t the type of the parameter. (REG_INT, REG_FLOAT or REG_DBL).
   * @param min the minimum value that a parameter can be steered to (can
   * be empty but not null).
   * @param max the maximum value that a parameter can be steered to (can
   * be empty but not null).
   */
  public ReG_SteerParameter(String l, boolean st, int t, String min, String max) {
    label = l;
    steerable = st;
    type = t;
    minLabel = min;
    maxLabel = max;

    switch(type) {
    case REG_INT:
      param = new Intp();
      break;
    case REG_CHAR:
      System.out.println("Parameters of type REG_CHAR, not supported in java. Sorry.");
      System.exit(REG_FAILURE);
      break;
    case REG_FLOAT:
      param = new Floatp();
      break;
    case REG_DBL:
      param = new Doublep();
      break;
    default:
      param = null;
      break;
    }
  }

  /**
   * Constructs a RealityGrid Steered Parameter of the requested type
   * and sets its value.
   *
   * @param l a descriptive label for the parameter.
   * @param v the value of the parameter.
   * @param st set the parameter to be steerable (if true) or just
   * monitored (if false).
   * @param min the minimum value that a parameter can be steered to (can
   * be empty but not null).
   * @param max the maximum value that a parameter can be steered to (can
   * be empty but not null).
   */
  public ReG_SteerParameter(String l, int v, boolean st, String min, String max) {
    this(l, st, REG_INT, min, max);
    ((Intp) param).assign(v);
  }

  /**
   * Constructs a RealityGrid Steered Parameter of the requested type
   * and sets its value.
   *
   * @param l a descriptive label for the parameter.
   * @param v the value of the parameter.
   * @param st set the parameter to be steerable (if true) or just
   * monitored (if false).
   * @param min the minimum value that a parameter can be steered to (can
   * be empty but not null).
   * @param max the maximum value that a parameter can be steered to (can
   * be empty but not null).
   */
  public ReG_SteerParameter(String l, float v, boolean st, String min, String max) {
    this(l, st, REG_FLOAT, min, max);
    ((Floatp) param).assign(v);
  }

  /**
   * Constructs a RealityGrid Steered Parameter of the requested type
   * and sets its value.
   *
   * @param l a descriptive label for the parameter.
   * @param v the value of the parameter.
   * @param st set the parameter to be steerable (if true) or just
   * monitored (if false).
   * @param min the minimum value that a parameter can be steered to (can
   * be empty but not null).
   * @param max the maximum value that a parameter can be steered to (can
   * be empty but not null).
   */
  public ReG_SteerParameter(String l, double v, boolean st, String min, String max) {
    this(l, st, REG_DBL, min, max);
    ((Doublep) param).assign(v);
  }

  /**
   * Register this parameter with the steering library.
   *
   * @throws ReG_SteerException If registering the parameter with the
   * steering library failed for some reason.
   * @see ReG_SteerAppside#registerParam(ReG_SteerParameter)
   */
  public void register() throws ReG_SteerException {
    int steered = REG_FALSE;
    if(steerable)
      steered = REG_TRUE;
    
    int status = ReG_Steer.Register_param(label, steered, getVoidPointer(),
					  type, minLabel, maxLabel);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to register parameter.", status);
    }
  }

  /**
   * Set the value of the steered parameter. If the type of the parameter
   * is not REG_INT the input value will be cast.
   *
   * @param p the new value of the parameter.
   * @see #setValue(float)
   * @see #setValue(double)
   */
  public void setValue(int p) {  
    switch(type) {
    case REG_INT:
      ((Intp) param).assign(p);
      break;
    case REG_FLOAT:
      ((Floatp) param).assign(new Integer(p).floatValue());
      break;
    case REG_DBL:
      ((Doublep) param).assign(new Integer(p).doubleValue());
      break;
    }
  }

  /**
   * Set the value of the steered parameter. If the type of the parameter
   * is not REG_FLOAT the input value will be cast.
   *
   * @param p the new value of the parameter.
   * @see #setValue(int)
   * @see #setValue(double)
   */
  public void setValue(float p) {
    switch(type) {
    case REG_INT:
      ((Intp) param).assign(new Float(p).intValue());
      break;
    case REG_FLOAT:
      ((Floatp) param).assign(p);
      break;
    case REG_DBL:
      ((Doublep) param).assign(new Float(p).doubleValue());
      break;
    }
  }

  /**
   * Set the value of the steered parameter. If the type of the parameter
   * is not REG_DBL the input value will be cast.
   *
   * @param p the new value of the parameter.
   * @see #setValue(int)
   * @see #setValue(float)
   */
  public void setValue(double p) {
    switch(type) {
    case REG_INT:
      ((Intp) param).assign(new Double(p).intValue());
      break;
    case REG_FLOAT:
      ((Floatp) param).assign(new Double(p).floatValue());
      break;
    case REG_DBL:
      ((Doublep) param).assign(p);
      break;
    }
  }

  /**
   * Get the current value of the parameter as an integer. If the parameter
   * is not of type REG_INT it will be cast.
   *
   * @return the integer value of the parameter.
   * @see #getValue()
   * @see #getFloatValue()
   * @see #getDoubleValue()
   */
  public int getIntValue() {
    int result = 0;

    switch(type) {
    case REG_INT:
      result = ((Intp) param).value();
      break;
    case REG_FLOAT:
      result = (new Float(((Floatp) param).value())).intValue();
      break;
    case REG_DBL:
      result = (new Double(((Doublep) param).value())).intValue();
      break;
    }

    return result;
  }

  /**
   * Get the current value of the parameter as a float. If the parameter
   * is not of type REG_FLOAT it will be cast.
   *
   * @return the float value of the parameter.
   * @see #getValue()
   * @see #getIntValue()
   * @see #getDoubleValue()
   */
  public float getFloatValue() {
    float result = 0.0f;

    switch(type) {
    case REG_INT:
      result = (new Integer(((Intp) param).value())).floatValue();
      break;
    case REG_FLOAT:
      result = ((Floatp) param).value();
      break;
    case REG_DBL:
      result = (new Double(((Doublep) param).value())).floatValue();
      break;
    }

    return result;
  }

  /**
   * Get the current value of the parameter as a double. If the parameter
   * is not of type REG_DBL it will be cast.
   *
   * @return the double value of the parameter.
   * @see #getValue()
   * @see #getIntValue()
   * @see #getFloatValue()
   */
  public double getDoubleValue() {
    double result = 0.0;

    switch(type) {
    case REG_INT:
      result = (new Integer(((Intp) param).value())).doubleValue();
      break;
    case REG_FLOAT:
      result = (new Float(((Floatp) param).value())).doubleValue();
      break;
    case REG_DBL:
      result = ((Doublep) param).value();
      break;
    }

    return result;
  }

  /**
   * Get the current value of the parameter wrapped in an Object.
   *
   * @return the current value of the parameter.
   * @see #getIntValue()
   * @see #getFloatValue()
   * @see #getDoubleValue()
   */
  public Object getValue() {
    Object result;

    switch(type) {
    case REG_INT:
      result = new Integer(((Intp) param).value());
      break;      
//     case REG_CHAR:
//       result = new Character(((Charp) param).value());
//       break;      
    case REG_FLOAT:
      result = new Float(((Floatp) param).value());
      break;      
    case REG_DBL:
      result = new Double(((Doublep) param).value());
      break;      
    default:
      result = null;
      break;
    }

    return result;
  }

  /**
   * Provides a C/C++ style void pointer to this parameter. This is actually
   * a pointer to the internal SWIG type used by this class rather than a
   * pointer to itself.
   *
   * @return the void pointer to this object.
   */
  public SWIGTYPE_p_void getVoidPointer() {
    SWIGTYPE_p_void result;

    switch(type) {
    case REG_INT:
      result = ((Intp) param).cast().getVoidPointer();
      break;
//     case REG_CHAR:
//       result = ((Charp) param).cast().getVoidPointer();
//       break;
    case REG_FLOAT:
      result = ((Floatp) param).cast().getVoidPointer();
      break;
    case REG_DBL:
      result = ((Doublep) param).cast().getVoidPointer();
      break;
    default:
      result = null;
      break;
    }

    return result;
  }

  /**
   * Get the text label of this parameter.
   *
   * @return the text label of this parameter.
   */
  public String getLabel() {
    return label;
  }

  /**
   * Determine whether this parameter is steerable or not.
   *
   * @return true if steerable, false otherwise.
   */
  public boolean isSteerable() {
    return steerable;
  }

  /**
   * Get the type of this parameter. This will be REG_INT, REG_FLOAT or
   * REG_DBL.
   *
   * @return the type of this parameter. (REG_INT, REG_FLOAT or
   * REG_DBL.)
   */
  public int getType() {
    return type;
  }

  /**
   * Get the minimum label for this parameter.
   *
   * @return the minimum text label for this parameter.
   */
  public String getMinLabel() {
    return minLabel;
  }

  /**
   * Get the maximum label for this parameter.
   *
   * @return the maximum text label for this parameter.
   */
  public String getMaxLabel() {
    return maxLabel;
  }

}
