/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  steerside WSRF, SOAP-based communication.

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
---------------------------------------------------------------------------*/
#ifndef __REG_STEER_STEERSIDE_WSRF_H__
#define __REG_STEER_STEERSIDE_WSRF_H__

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"

/** @file ReG_Steer_Steerside_WSRF.h
    @brief Header file for WSRF over SOAP communications for the steering client.
    @author Andrew Porter
*/

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

/*-------------------------------------------------------------------*/

/** @internal
    @param soap Pointer to soap runtime structure
    @param tag The unexpected tag

    (For debugging.) Handler passed to gSoap to be called when parser
    encounters unexpected tags 
*/
int soapMismatchHandler(struct soap *soap, 
			const char  *tag);

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param SimID Endpoint of the Steering Web Service representing
    the simulation

    Attach to a simulation represented by a SWS
*/
int Sim_attach_wsrf (Sim_entry_type *sim, 
                     char           *SimID);

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param buf The control message to send

    Send a control msg to a simulation 
*/
int Send_control_msg_wsrf (Sim_entry_type *sim,
			   char           *buf);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Send a detach msg to a simulation */
int Send_detach_msg_wsrf (Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Get the next status message from the simulation 
*/
struct msg_struct *Get_status_msg_wsrf(Sim_entry_type *sim);

/** @internal 
    @param sim Pointer to entry in main Sim_table
    @return NULL if no stored msg otherwise ptr to next message.

    Retrieve the next stored message (if any) from the simulation.
*/
struct msg_struct *Get_next_stored_msg(Sim_entry_type *sim);

/** @param soapStruct Pointer to soap struct
    @param epr The EPR of the Steering Service to contact
    @param username WSSE username (if any)
    @param passwd  WSSE passphrase (if any)
    @param name The name of the RP to get
    @param pRP If successful, ptr to array of char holding value of 
    RP (will be free'd when soap_end called on @p soapStruct) 

    Get the value of the specified resource property. Set @p username
    to NULL or an empty string to turn off use of WSSE.
*/
extern PREFIX int Get_resource_property (struct soap *soapStruct,
					 const char  *epr,
					 const char  *username,
					 const char  *passwd,
					 const char  *name,
					 char       **pRP);

/** @param soapStruct Pointer to initalised gSoap soap struct
    @param epr The EndPointReference of the SWS to query
    @param username  WSSE username (if any)
    @param passwd WSSE passphrase (if any)
    @param pRPDoc   If successful, ptr to array of char holding 
    contents of the ResourceProperty document (will be free'd when
    soap_end called on @p soapStruct) 

    Get the whole resource property document. Set @p username
    to NULL or an empty string to turn off use of WSSE.
*/
extern PREFIX int Get_resource_property_doc(struct soap *soapStruct,
					    const char  *epr,
					    const char  *username,
					    const char  *passwd,
					    char       **pRPDoc);

/** @param soapStruct Pointer to initalised gSoap soap struct
    @param epr The EndPointReference of the SWS to query
    @param username WSSE username (if any)
    @param passwd WSSE passphrase (if any)
    @param input String to use as arg. for remote call, of form
    '\<RPName\>value of RP\</RPName\>'.
    @returns REG_SUCCESS if call succeeds, REG_FAILURE otherwise

    Calls the SetResourceProperty method and passes the supplied buffer
    as input to call. Set @p username
    to NULL or an empty string to turn off use of WSSE. 
*/
extern PREFIX int Set_resource_property (struct soap *soapStruct,
					 const char  *epr,
					 const char  *username,
					 const char  *passwd,
					 char        *input);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Clean up a WSRF-based steering connection 
*/
int Finalize_connection_wsrf (Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param handle Handle of the parameter to get the log of
    @return REG_SUCCESS or REG_FAILURE.

    Retrieve the full log of the parameter with the specified @p handle.
    If successful then log data is stored in internal buffer.
*/
int Get_param_log_wsrf(Sim_entry_type *sim,
		       int             handle);

#endif
