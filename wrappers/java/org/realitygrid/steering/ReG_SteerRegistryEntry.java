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

package org.realitygrid.steering;

/**
 * This class provides information on a registry entry
 * as returned by various query routines.
 *
 * @version 2.0
 * @author Robert Haines
 */
public class ReG_SteerRegistryEntry implements ReG_SteerConstants {

    private String serviceType;
    private String GSH;
    private String entryGSH;
    private String application;
    private String startDateTime;
    private String user;
    private String group;
    private String jobDescription;

    public ReG_SteerRegistryEntry() {}

    public ReG_SteerRegistryEntry(String type,
				  String gsh,
				  String egsh,
				  String app,
				  String start,
				  String user,
				  String group,
				  String description) {
	serviceType = type;
	GSH = gsh;
	entryGSH = egsh;
	application = app;
	startDateTime = start;
	this.user = user;
	this.group = group;
	jobDescription = description;
    }

    public String getServiceType() {
	return serviceType;
    }

    public String getGSH() {
	return GSH;
    }

    public String getEntryGSH() {
	return entryGSH;
    }

    public String getApplication() {
	return application;
    }

    public String getStartDateTime() {
	return startDateTime;
    }

    public String getUser() {
	return user;
    }

    public String getGroup() {
	return group;
    }

    public String getJobDescription() {
	return jobDescription;
    }
}
