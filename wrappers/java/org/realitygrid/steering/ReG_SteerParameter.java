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

public class ReG_SteerParameter implements ReG_SteerConstants {

  private String label;
  private int type;
  private boolean steerable;
  private String minLabel;
  private String maxLabel;

  private Object param;

  /**
   * cons
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

  public ReG_SteerParameter(String l, int v, boolean st, int t, String min, String max) {
    this(l, st, t, min, max);
    ((Intp) param).assign(v);
  }

  public ReG_SteerParameter(String l, float v, boolean st, int t, String min, String max) {
    this(l, st, t, min, max);
    ((Floatp) param).assign(v);
  }

  public ReG_SteerParameter(String l, double v, boolean st, int t, String min, String max) {
    this(l, st, t, min, max);
    ((Doublep) param).assign(v);
  }

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

  public void setValue(int p) {
    if(type == REG_INT)
      ((Intp) param).assign(p);
    else
      ((Intp) param).assign(0);
  }

  public void setValue(float p) {
    if(type == REG_FLOAT)
      ((Floatp) param).assign(p);
    else
      ((Floatp) param).assign(0.0f);
  }

  public void setValue(double p) {
    if(type == REG_DBL)
      ((Doublep) param).assign(p);
    else
      ((Doublep) param).assign(0.0);
  }

  public int getIntValue() {
    int result = 0;
    if(type == REG_INT) 
      result = ((Intp) param).value();

    return result;
  }

  public float getFloatValue() {
    float result = 0;
    if(type == REG_FLOAT) 
      result = ((Floatp) param).value();

    return result;
  }

  public double getDoubleValue() {
    double result = 0;
    if(type == REG_DBL) 
      result = ((Doublep) param).value();

    return result;
  }

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

  public String getLabel() {
    return label;
  }

  public boolean isSteerable() {
    return steerable;
  }

  public int getType() {
    return type;
  }

  public String getMinLabel() {
    return minLabel;
  }

  public String getMaxLabel() {
    return maxLabel;
  }

}
