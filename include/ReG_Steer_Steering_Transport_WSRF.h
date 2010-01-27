/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Robert Haines
 */

#ifndef __REG_STEER_STEERING_TRANSPORT_WSRF_H__
#define __REG_STEER_STEERING_TRANSPORT_WSRF_H__

/** @file ReG_Steer_Steering_Transport_WSRF.h
 *  @brief WSRF specific routines for the steering transport module.
 *
 *  @author Robert Haines
 */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Steering_Transport_API.h"
#include "ReG_Steer_XML.h"
#include "soapH.h"

#define REG_APPSIDE_WSSE_USERNAME "application"

/** @internal
    For steering via SOAP - holds info on the Grid/Web Service */
typedef struct {
  /** whether we're steering via SOAP (1) or not (0) */
  int  active;
  /** Location of the Grid service */
  char address[REG_MAX_STRING_LENGTH];
  /** Holds list of names of service data elements on the SGS for which
      notifications are pending */
  char notifications[REG_MAX_NUM_SGS_SDE][REG_MAX_STRING_LENGTH];
  /** No. of notifications that we've yet to process (OGSI) */
  int  sde_count;
  /** Our current position in the @p notifications array ?? (OGSI) */
  int  sde_index;
  /** The structure holding the gSoap environment for this connection */
  struct soap *soap;
  /** Username to use with WS-Security with this service */
  char username[REG_MAX_STRING_LENGTH];
  /** Passphrase (if any) used with WS-Security for this service */
  char passwd[REG_MAX_STRING_LENGTH];
  /** The last-modified time of the ResourceProperty document of our
      associated SWS when we last looked at it (WSRF) */
  long int lastModTime;

} SGS_info_type;

typedef struct {
  int max_entries;
  int num_used;
  SGS_info_type* SGS_info;
} SGS_info_table_type;

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
int get_resource_property(struct soap *soapStruct,
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
int get_resource_property_doc(struct soap *soapStruct,
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
int set_resource_property(struct soap *soapStruct,
			  const char  *epr,
			  const char  *username,
			  const char  *passwd,
			  char        *input);

/** @internal
    @param sim Pointer to entry in main Sim_table
    @return NULL if no stored msg otherwise ptr to next message.

    Retrieve the next stored message (if any) from the simulation.
*/
struct msg_struct* get_next_stored_msg(Sim_entry_type* sim);

/** @internal
    Creates a WSRF header including WS-Security elements  for gSoap
    (within the supplied soap struct). If @p username is null or
    is empty then no WS-Security elements are created.
    @param aSoap Pointer to soap struct to construct header in
    @param epr The address of the service to be called
    @param username The username to present to the service
    @param passwd The password used to access the service
    @return REG_SUCCESS or REG_FAILURE if no header created */
int create_WSRF_header(struct soap *aSoap,
		       const  char *epr,
		       const  char *username,
		       const  char *passwd);

/** @internal
    Calls the Destroy method on the service at the supplied Endpoint.
    Note that an SWS is derived from a WSRP so this method applies
    to SWSs.
    @param epr Endpoint reference of the service to destroy
    @param sec Pointer to struct holding WSSE username & password
    (if any) and necessary details for SSL (if being used) */
int destroy_WSRP(const char *epr,
		 const struct reg_security_info *sec);

/** @internal
    Initialize the OpenSSL random number generator for this thread */
int init_ssl_random_seq();

/** @internal
    Initialize the SSL context for the supplied gSoap structure
    @param aSoap Ptr to soap struct to be initialized
    @param authenticateSWS Whether or not to verify the cert. presented
    by the SWS.  If REG_TRUE then caCertPath must be set.
    @param certKeyPemFile Full path to file containing user's certificate
           and key (if doing mutual authentication, NULL otherwise)
    @param passphrase Passphrase for the user's key (can be NULL if not
           doing mutual authentication)
    @param caCertPath Path to directory containing CA certs
    @return REG_SUCCESS or REG_FAILURE */
int init_ssl_context(struct soap *aSoap,
		     const int    authenticateSWS,
		     const char  *certKeyPemFile,
		     const char  *passphrase,
		     const char  *caCertPath);

int SGS_info_table_init(SGS_info_table_type* table,
			const int max_entries);

int soap_mismatch_handler(struct soap* soap, const char* tag);

#endif /* __REG_STEER_STEERING_TRANSPORT_WSRF_H__ */
