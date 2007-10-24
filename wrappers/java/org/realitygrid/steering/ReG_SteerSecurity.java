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
 * This class provides security information for connecting to
 * registries and simulations.
 *
 * @version 2.0
 * @author Robert Haines
 */
public class ReG_SteerSecurity implements ReG_SteerConstants {

    private boolean usingSSL;
    private String  caCertsPath;
    private String  myKeyCertFile;
    private String  userDN;
    private String  passphrase;

    public ReG_SteerSecurity() {
	clear();
    }

    public ReG_SteerSecurity(boolean ssl, String caPath, String keyfile, String dn, String pass) {
	usingSSL = ssl;
	caCertsPath = caPath;
	myKeyCertFile = keyfile;
	userDN = dn;
	passphrase = pass;
    }

    public ReG_SteerSecurity(String filename) {
	ReG_SteerSecurity temp;
	temp = (ReG_SteerSecurity) ReG_Steer.Get_security_config_j(filename);

	usingSSL = temp.isUsingSSL();
	caCertsPath = temp.getCaCertsPath();
	myKeyCertFile = temp.getMyKeyCertFile();
	userDN = temp.getUserDN();
	passphrase = temp.getPassphrase();
    }

    public boolean isUsingSSL() {
	return usingSSL;
    }

    public void setUsingSSL(boolean ssl) {
	usingSSL = ssl;
    }

    public String getCaCertsPath() {
	return caCertsPath;
    }

    public void setCaCertsPath(String caPath) {
	caCertsPath = caPath;	
    }

    public String getMyKeyCertFile() {
	return myKeyCertFile;
    }

    public void setMyKeyCertFile(String keyfile) {
	myKeyCertFile = keyfile;
    }

    public String getUserDN() {
	return userDN;
    }

    public void setUserDN(String dn) {
	userDN = dn;
    }

    public String getPassphrase() {
	return passphrase;
    }

    public void setPassphrase(String pass) {
	passphrase = pass;
    }

    public void clear() {
	usingSSL = false;
	caCertsPath = "";
	myKeyCertFile = "";
	userDN = "";
	passphrase = "";	
    }

    public String toString() {
	String result = "Using SSL: " + usingSSL + "\n";
	result += "CA Certs Path: " + caCertsPath + "\n";
	result += "User Cert and Key File: " + myKeyCertFile + "\n";
	result += "User DN: " + userDN + "\nPassphrase: ";

	if(passphrase.length() > 0)
	    result += "**************";
	else
	    result += "<not set>";

	return result;
    }
}
