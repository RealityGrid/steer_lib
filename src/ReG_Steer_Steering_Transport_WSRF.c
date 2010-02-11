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

/** @internal
    @file ReG_Steer_Steering_Transport_WSRF.c
    @brief Source file for WSRF-based steering transport.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Steering_Transport_API.h"
#include "ReG_Steer_Steering_Transport_WSRF.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "Base64.h"
#include "soapH.h"

/** @internal
   Flag holding whether or not the ssl random no. generator has
   been initialized. */
int ReG_ssl_random_initialized;

/* */
SGS_info_type appside_SGS_info;

/** @internal
   The table holding details of our communication channel with the
   steering client - defined in ReG_Steer_Appside.c */
extern Steerer_connection_table_type Steerer_connection;

/** Log of values of parameters for which logging has
   been requested */
extern Chk_log_type Param_log;

extern Param_table_type Params_table;

/** Table for registered checkpoint types */
extern IOdef_table_type ChkTypes_table;

extern Chk_log_type Chk_log;

/* Actual declaration is in ReG_Steer_Appside.c */
extern struct msg_store_struct  Msg_store;
extern struct msg_store_struct *Msg_store_tail;

/** Basic library config - declared in ReG_Steer_Common */
extern Steer_lib_config_type Steer_lib_config;

/* Hostname of machine we are executing on */
char ReG_Hostname[REG_MAX_STRING_LENGTH];

/* Name (and version) of the application that has called us */
extern char ReG_AppName[REG_MAX_STRING_LENGTH];

/** Names of the SWS' ResourceProperties - MUST match those
    used in SWS.pm (as launched by container) */
char *SUPPORTED_CMDS_RP = "sws:supportedCommands";
/** @see SUPPORTED_CMDS_SDE */
char *PARAM_DEFS_RP     = "sws:paramDefinitions";
/** @see SUPPORTED_CMDS_SDE */
char *IOTYPE_DEFS_RP    = "sws:ioTypeDefinitions";
/** @see SUPPORTED_CMDS_SDE */
char *CHKTYPE_DEFS_RP   = "sws:chkTypeDefinitions";
/** @see SUPPORTED_CMDS_SDE */
char *STEER_STATUS_RP   = "sws:steererStatus";
/** @see SUPPORTED_CMDS_SDE */
char *MACHINE_ADDRESS_RP= "sws:machineAddress";
/** @see SUPPORTED_CMDS_SDE */
char *WORKING_DIR_RP    = "sws:workingDirectory";
/** @see SUPPORTED_CMDS_SDE */
char *APP_NAME_RP       = "sws:applicationName";
/** @see SUPPORTED_CMDS_SDE */
char *STATUS_MSG_RP     = "sws:statusMsg";

/*----------------- Appside methods ---------------------*/

int Initialize_steering_connection_impl(const int  NumSupportedCmds,
					int* SupportedCmds) {
  char* pchar;
  char* ip_addr;
  char  query_buf[REG_MAX_MSG_SIZE];
  struct wsrp__SetResourcePropertiesResponse out;

  strncpy(Steer_lib_config.Steering_transport_string, "WSRF", 5);

  /* malloc memory for soap struct for this connection and then
     initialise it */
  if(!(appside_SGS_info.soap =
                (struct soap*)malloc(sizeof(struct soap)))) {

    fprintf(stderr, "STEER: Initialize_steering_connection: failed"
	    " to malloc memory for soap struct\n");
    return REG_FAILURE;
  }

  /* Get the address of the SWS for this application from an environment
     variable */
  if((pchar = getenv("REG_SGS_ADDRESS"))) {
    snprintf(appside_SGS_info.address, REG_MAX_STRING_LENGTH,
	     "%s", pchar);
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Initialize_steering_connection: "
	    "SWS address = %s\n", appside_SGS_info.address);
#endif
  }
  else {
    fprintf(stderr, "STEER: Initialize_steering_connection: "
	    "REG_SGS_ADDRESS environment variable not set\n");
    return REG_FAILURE;
  }

  /* Initialise the soap run-time environment:
     Use this form to turn-on keep-alive for both incoming and outgoing
     http connections */
  soap_init2(appside_SGS_info.soap, SOAP_IO_KEEPALIVE,
	     SOAP_IO_KEEPALIVE);

  /* If address of SWS begins with 'https' then initialize SSL context */
  if(strstr(appside_SGS_info.address, "https") ==
     appside_SGS_info.address) {
    if(init_ssl_context(appside_SGS_info.soap,
			REG_FALSE, /* Don't authenticate SWS */
			NULL,/*char *certKeyPemFile,*/
			NULL, /* char *passphrase,*/
			NULL/*CA certs directory */) == REG_FAILURE) {
      fprintf(stderr, "STEER: ERROR: Initialize_steering_connection: "
	      "call to initialize soap SSL context failed\n");
      return REG_FAILURE;
    }
  }

  /* Initialize the OpenSSL random no. generator and, if successful,
     get the passphrase, if any, for the SWS (for use with WS-Security) */
  appside_SGS_info.passwd[0] = '\0';
  snprintf(appside_SGS_info.username, REG_MAX_STRING_LENGTH,
	   "%s", REG_APPSIDE_WSSE_USERNAME);

  if(init_ssl_random_seq() == REG_SUCCESS) {
    if((pchar = getenv("REG_PASSPHRASE"))) {
      snprintf(appside_SGS_info.passwd, REG_MAX_STRING_LENGTH,
	       "%s", pchar);
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Initialize_steering_connection: "
	      "passphrase read OK\n");
#endif
    }
    else {
      fprintf(stderr, "STEER: Initialize_steering_connection: "
	      "no passphrase available from REG_PASSPHRASE\n");
    }
  }
  else {
      fprintf(stderr, "STEER: Initialize_steering_connection: "
	      "failed to initialize OpenSSL random no. generator\n");
  }

  /* Since we are using KEEPALIVE, we can also ask gSOAP to bind the
     socket to a specific port on the local machine - only do this if
     GLOBUS_TCP_PORT_RANGE is set. */
  if((pchar = getenv("GLOBUS_TCP_PORT_RANGE"))) {
    if(sscanf(pchar, "%d,%d",
	      &(appside_SGS_info.soap->client_port_min),
	      &(appside_SGS_info.soap->client_port_max)) != 2) {
      appside_SGS_info.soap->client_port_min = 0;
      appside_SGS_info.soap->client_port_max = 0;
    }
  }

  /* Get the hostname of this machine - used to tell the outside
     world how to contact us and where the (checkpoint) files we've
     written live.  On many machines we may be running on a node
     that cannot be seen by the outside world and therefore globus
     connections for file transfer etc. will need an address of a
     publicly-accessible node.  This can be passed to us via the
     REG_MACHINE_NAME environment variable.
  */
  if((pchar = getenv("REG_MACHINE_NAME"))) {
    strncpy(ReG_Hostname, pchar, REG_MAX_STRING_LENGTH);
  }
  else if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS) {
    strncpy(ReG_Hostname, pchar, REG_MAX_STRING_LENGTH);
  }
  else {
    ReG_Hostname[0] = '\0';
  }

  /* Create msg to send to SGS */
  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds,
		     Steerer_connection.supp_cmds, REG_MAX_MSG_SIZE);

  /* Strip off any xml version declaration */
  pchar = strstr(Steerer_connection.supp_cmds,"<ReG_steer_message");

  snprintf(query_buf, REG_MAX_MSG_SIZE, "<%s>%s</%s>",
	  SUPPORTED_CMDS_RP, pchar, SUPPORTED_CMDS_RP);

  create_WSRF_header(appside_SGS_info.soap,
                     appside_SGS_info.address,
                     appside_SGS_info.username,
		     appside_SGS_info.passwd);

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Initialize_steering_connection: sending "
	  "1st msg:\n>>%s<<\n\n",query_buf);
#endif

  if(soap_call_wsrp__SetResourceProperties(appside_SGS_info.soap,
					   appside_SGS_info.address,
					   "", query_buf, &out) != SOAP_OK) {
    fprintf(stderr, "STEER: Initialize_steering_connection: failed "
	    "to set supportedCommands ResourceProperty:\n");
    soap_print_fault(appside_SGS_info.soap, stderr);
    return REG_FAILURE;
  }

  /* Publish our location: machine and working directory - these
     are set in Steering_initialize prior to calling us */
  snprintf(query_buf, REG_MAX_MSG_SIZE,
	   "<%s>%s</%s><%s>%s</%s><%s>%s</%s>",
	   WORKING_DIR_RP, Steer_lib_config.working_dir, WORKING_DIR_RP,
	   MACHINE_ADDRESS_RP, ReG_Hostname, MACHINE_ADDRESS_RP,
	   APP_NAME_RP, ReG_AppName, APP_NAME_RP);

  create_WSRF_header(appside_SGS_info.soap,
                     appside_SGS_info.address,
		     appside_SGS_info.username,
		     appside_SGS_info.passwd);

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Initialize_steering_connection: sending "
	  "2nd msg:\n>>%s<<\n\n", query_buf);
#endif

  if(soap_call_wsrp__SetResourceProperties(appside_SGS_info.soap,
					   appside_SGS_info.address,
					   "", query_buf, &out) != SOAP_OK) {
    fprintf(stderr, "STEER: Initialize_steering_connection: failed to "
	    "set machine address and working directory ResourceProperties:\n");
    soap_print_fault(appside_SGS_info.soap, stderr);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Finalize_steering_connection_impl() {
  int return_status = REG_SUCCESS;
  struct wsrp__DestroyResponse out;
  int  commands[1];

  commands[0] = REG_STR_DETACH;
  Emit_status(0,
	      0,
	      NULL,
	      1,
	      commands);

  create_WSRF_header(appside_SGS_info.soap,
                     appside_SGS_info.address,
		     appside_SGS_info.username,
		     appside_SGS_info.passwd);

  if(soap_call_wsrp__Destroy(appside_SGS_info.soap,
			    appside_SGS_info.address,
			    "",  NULL, &out)) {
    fprintf(stderr, "STEER: Finalize_steering_connection_wsrf: call to Destroy"
	    " failed:\n");
    soap_print_fault(appside_SGS_info.soap, stderr);
    return_status = REG_FAILURE;
  }

  soap_end(appside_SGS_info.soap);
  /* Reset: close master/slave sockets and remove callbacks */
  soap_done(appside_SGS_info.soap);

  free(appside_SGS_info.soap);
  appside_SGS_info.soap = NULL;

  return return_status;
}

/*-------------------------------------------------------*/

int Detach_from_steerer_impl() {
  /* Don't send out param logs as they are cached on the SWS */
  Param_log.send_all         = REG_FALSE;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Steerer_connected_impl() {
  char* steer_status;
  int   status;

  status = get_resource_property(appside_SGS_info.soap,
				 appside_SGS_info.address,
				 appside_SGS_info.username,
				 appside_SGS_info.passwd,
				 STEER_STATUS_RP,
				 &steer_status);
  if(status != REG_SUCCESS) {
    fprintf(stderr, "STEER: Steerer_connected_wsrf: get_resource_property failed\n");
    return REG_FAILURE;
  }
#ifdef REG_DEBUG
  else{
    fprintf(stderr, "STEER: Steerer_connected_wsrf: get_resource_property "
	    "returned: %s\n", steer_status);
  }
#endif

  if(strstr(steer_status, "ATTACHED")) {
    return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------*/

int Send_status_msg_impl(char* msg) {
  char*  sde_name;
  char   query_buf[REG_MAX_MSG_SIZE];
  int    nbytes;
  int    new_size;
  int    loopCount;
  int    status;
  /*  struct wsrp__SetResourcePropertiesResponse out;*/
  char*  pTmpBuf;
  char*  pbuf;

  /* Status & log messages are both sent as 'status' messages */
  if(strstr(msg, "<App_status>") || strstr(msg, "<Steer_log>")) {
    sde_name = STATUS_MSG_RP;
  }
  else if(strstr(msg, "<Supported_commands>")) {
    sde_name = SUPPORTED_CMDS_RP;
  }
  else if(strstr(msg, "<Param_defs>")) {
    sde_name = PARAM_DEFS_RP;
  }
  else if(strstr(msg, "<IOType_defs>")) {
    sde_name = IOTYPE_DEFS_RP;
  }
  else if(strstr(msg, "<ChkType_defs>")) {
    sde_name = CHKTYPE_DEFS_RP;
  }
  else {
    fprintf(stderr, "STEER: Send_status_msg_wsrf: not a status or log msg and"
	    "no matching SDE name found either\n");
    return REG_FAILURE;
  }

  pbuf = NULL;
  pTmpBuf = query_buf;
  nbytes = snprintf(query_buf, REG_MAX_MSG_SIZE, "<%s>%s</%s>",
		    sde_name, msg, sde_name);

  /* Check for truncation - if it occurs then malloc a bigger buffer
     and try again */
  if((nbytes >= (REG_MAX_MSG_SIZE-1)) || (nbytes < 1)) {
    new_size = strlen(msg) + 512;
    if(!(pbuf = (char *)malloc(new_size))) {
      fprintf(stderr, "STEER: Send_status_msg_wsrf: malloc failed\n");
      return REG_FAILURE;
    }

    nbytes = snprintf(pbuf, new_size, "<%s>%s</%s>",
		      sde_name, msg, sde_name);

    if((nbytes >= (new_size-1)) || (nbytes < 1)) {
      free(pbuf);
      pbuf = NULL;
      fprintf(stderr, "STEER: ERROR: Send_status_msg_wsrf: msg truncated\n");
      return REG_FAILURE;
    }
    pTmpBuf = pbuf;
  }

  /* We loop until we have a clear-cut success or failure - i.e.
     if we have a deadlock we fall back to here and try again */
  loopCount = -1;
  while(1 && (loopCount < 10)) {
    loopCount++;
    status = set_resource_property(appside_SGS_info.soap,
				   appside_SGS_info.address,
				   appside_SGS_info.username,
				   appside_SGS_info.passwd,
				   pTmpBuf);
    if(status == REG_TIMED_OUT) {
      continue;
    }
    else if(status != REG_SUCCESS) {
      if(pbuf) free(pbuf);
      return REG_FAILURE;
    }
    else {
      break;
    }
  }

  if(pbuf) {
    free(pbuf);
    pbuf = NULL;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

struct msg_struct* Get_control_msg_impl() {
  struct msg_struct* msg = NULL;
  char* pBuf;
  char* pLast;
  char* pStart;

  /* If we have a backlog of messages then return the next one
     - we are only interested in control messages */
  while((msg = get_next_stored_msg(NULL))) {
    if(msg->control) {
      return msg;
    }
    Delete_msg_struct(&msg);
  }

  /* Get any new control messages */
  if(get_resource_property(appside_SGS_info.soap,
			   appside_SGS_info.address,
			   appside_SGS_info.username,
			   appside_SGS_info.passwd,
			   "controlMsg", &pBuf) != REG_SUCCESS) {
    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }

  /* Parse and store the control messages in the order in which they
     occur in the RPDoc which is the same as the order in which they
     were received. */
  if(!(pStart = strstr(pBuf, "<sws:controlMsg"))) {
    /* No control message found */
    return NULL;
  }

  while(pStart) {
    pLast = strstr(pStart+1, "<sws:controlMsg");
    /* Parse the doc - pass NULLs in as this is appside so have no
       Sim_entry struct and results will be put in Msg_store struct */
    if(pLast) {
      Parse_xml_buf(pStart, (int)(pLast-pStart), NULL, NULL);
    }
    else {
      /* Have reached last control message so length of this one
	 is just num. chars 'til end of buffer */
      Parse_xml_buf(pStart, strlen(pStart), NULL, NULL);
    }
    pStart = pLast;
  }

  /* The results of parsing the ResourcePropertyDocument are stored
     as a series of messages - go through these until we find a
     control message (if any) */
  msg = get_next_stored_msg(NULL);
  while(msg && !(msg->control)) {
    Delete_msg_struct(&msg);
    msg = get_next_stored_msg(NULL);
  }

  return msg;
}

/*-------------------------------------------------------*/

int Get_data_io_address_impl(const int           index,
			     const int           direction,
			     char*               hostname,
			     unsigned short int* port,
			     char*               label) {
  char  *pBuf;
  char  *pchar;
  char  *pLast;
  int    count;

  /* Port returned as zero on failure */
  *port = 0;

  if(direction == REG_IO_IN) {
    char  *pIOType;
    char  *pLabel;
    char   epr[REG_MAX_STRING_LENGTH];
    char   address[REG_MAX_STRING_LENGTH];
    struct soap mySoap;

    if(get_resource_property(appside_SGS_info.soap,
			     appside_SGS_info.address,
			     appside_SGS_info.username,
			     appside_SGS_info.passwd,
			     "dataSource", &pBuf) != REG_SUCCESS) {
      return REG_FAILURE;
    }
    /*
      <xs:element name="dataSource">
        <xs:complexType>
          <xs:choice>
            <xs:element name="Proxy"/>
            <xs:element name="address" type="xs:string"/>
            <xs:element name="port" type="xs:integer"/>
            <xs:element type="xs:string" name="sourceEPR"/>
          </xs:choice>
          <xs:element type="xs:string" name="sourceLabel"/>
        </xs:complexType>
      </xs:element>
    */
    epr[0]='\0';
    label[0]='\0';
    count = 1;
    pLast = pBuf;
    if(strstr(pLast, "<sourceEPR>")){
      while( (pLast = strstr(pLast, "<sws:dataSource")) ){
	if(count == index){
	  /* Pull out the EPR of the service that will provide
	     our data */
	  pchar = strstr(pLast, "<sourceEPR>");
	  pLabel = strstr(pLast, "<sourceLabel>");

	  pchar += strlen("<sourceEPR>");
	  pLast = strchr(pchar, '<');
	  count = pLast - pchar;
	  strncpy(epr, pchar, count);
	  epr[count]='\0';

	  /* Pull out the label of the IOType of that SWS that
	     will provide our data */
	  pchar = strstr(pLabel, "<sourceLabel>");
	  pchar += strlen("<sourceLabel>");
	  pLast = strchr(pchar, '<');
	  count = pLast - pchar;
	  strncpy(label, pchar, count);
	  label[count]='\0';
	  break;
	}
	pLast++;
      }

      if(strlen(epr) == 0 || strlen(label) == 0) {
	return REG_FAILURE;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Get_data_source_address_wsrf: Got EPR = %s\n"
	      "                                               label = %s\n",
	      epr, label);
#endif

      /* Set-up soap environment for call to a SWS other than our own */
      /* Initialise the soap run-time environment:
	 Use this form to turn-on keep-alive for both incoming and outgoing
	 http connections */
      soap_init2(&mySoap, SOAP_IO_KEEPALIVE,
		 SOAP_IO_KEEPALIVE);
      /* Set the endpoint address */
      snprintf(address, REG_MAX_STRING_LENGTH,
	       "%s", epr);
      /* Since we are using KEEPALIVE, we can also ask gSOAP to bind the
	 socket to a specific port on the local machine - only do this if
	 GLOBUS_TCP_PORT_RANGE is set. */
      mySoap.client_port_min = appside_SGS_info.soap->client_port_min;
      mySoap.client_port_max = appside_SGS_info.soap->client_port_max;

      /* We use the same username and password for the data-source SWS as we do
	 for 'our' SWS */
      if(get_resource_property(&mySoap, address,
			       appside_SGS_info.username,
			       appside_SGS_info.passwd,
			       "ioTypeDefinitions", &pBuf) != REG_SUCCESS){
	return REG_FAILURE;
      }

      /* Parse the IOtypes */

      pIOType = pBuf;
      while((pIOType = strstr(pIOType, "<IOType>"))) {
	/* According to schema, Label must occur before Address */
	pLast = strstr(pIOType, "<Label>");
	pLast += 7; /* strlen("<Label>") = 7 */
	pchar = strstr(pLast, "</Label>");
	*pchar = '\0';
	if(!strncmp(pLast, label, count)) {
	  /* This is the one we want */
	  *pchar = '<';
	  if( !(pLast = strstr(pIOType, "<Address>")) ){
	    fprintf(stderr, "STEER: Get_data_source_address_wsrf: ERROR: "
		    "IOType %s does not have an Address element\n",
		    label);
	    label[0] = '\0';
	    break;
	  }
	  pLast += 9; /* strlen("<Address>") = 9 */
	  pchar = strstr(pLast, "</Address>");
	  count = pchar - pLast;
	  strncpy(label, pLast, count);
	  label[count]='\0';
	  break;
	}
	*pchar = '<';
	pIOType++;
      }

      soap_end(&mySoap);
      /* Reset: close master/slave sockets and remove callbacks */
      soap_done(&mySoap);

      /* Parse the Address field to pull out port and host */
      if((pchar = strchr(label, ':'))) {
	strncpy(hostname, label, (pchar - label));
	hostname[(pchar - label)] = '\0';
	pchar++;
	*port = (unsigned short int)atoi(pchar);
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Get_data_source_address_wsrf: host = %s\n"
		"                                             port = %d\n",
		hostname, *port);
#endif
	return REG_SUCCESS;
      }

      fprintf(stderr, "STEER: Get_data_source_address_wsrf: failed to match "
	      "IOType label\n");
    }
    else { /* Using a proxy for IO */

      count = index; /* ARPDBG - remove check for multiple sources for now
			- we take the first we find. Effectively this means
			that an application can only use one IOProxy for
			input, irrespective of how many output IOTypes it has */

      while((pLast = strstr(pLast, "<sws:dataSource"))) {
	if(count == index){
	  /* Pull out the hostname and port of the proxy that will
	     provide our data */
	  pchar = strstr(pLast, "<Proxy>");
	  pchar = strstr(++pchar, "<address>");
	  pchar += 9; /* = strlen("<address>") */
	  pLast = strchr(pchar, '<');
	  count = pLast - pchar;
	  strncpy(hostname, pchar, count);
	  hostname[count]='\0';
	  pchar = strstr(pLast, "<port>");
	  pchar += 6; /* = strlen("<port>") */
	  pLast = strchr(pchar, '<');
	  count = pLast - pchar;
	  /* epr used as temp buffer here */
	  strncpy(epr, pchar, count);
	  epr[count]='\0';
	  *port = (unsigned short int)atoi(epr);
	  /* Pull out the label used to identify our data by
	     the proxy */
	  count = 0;
	  if( (pchar = strstr(pLast, "<sourceLabel>")) ){
	    pchar += 13; /* = strlen("<sourceLabel>") */
	    pLast = strchr(pchar, '<');
	    count = pLast - pchar;
	    strncpy(label, pchar, count);
	  }
	  label[count]='\0';
	  break;
	}
	pLast++;
      }
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Get_data_source_address_wsrf: proxy host = %s\n"
	      "                                     proxy port = %d\n"
	      "                                          label = %s\n",
	      hostname, *port, label);
#endif /* REG_DEBUG */
    } /* end if(strstr("sourceEPR")) */

  }

  /* direction == REG_IO_OUT */
  else {
    char   tmpBuf[REG_MAX_STRING_LENGTH];

    if(get_resource_property(appside_SGS_info.soap,
			     appside_SGS_info.address,
			     appside_SGS_info.username,
			     appside_SGS_info.passwd,
			     "dataSink", &pBuf) != REG_SUCCESS){
      return REG_FAILURE;
    }

    pLast = pBuf;
    count = index; /* ARPDBG - remove check for multiple sinks for now
		      - we take the first we find. Effectively this means
		      that an application can only use one IOProxy for
		      output, irrespective of how many output IOTypes it has */
    if(strstr(pLast, "<Proxy>")){
      while( (pLast = strstr(pLast, "<sws:dataSink")) ){
	if(count == index){
	  /* Pull out the hostname and port of the proxy that will
	     provide our data */
	  pchar = strstr(pLast, "<Proxy>");
	  pchar = strstr(++pchar, "<address>");
	  pchar += 9; /* = strlen("<address>") */
	  pLast = strchr(pchar, '<');
	  count = pLast - pchar;
	  strncpy(hostname, pchar, count);
	  hostname[count]='\0';
	  pchar = strstr(pLast, "<port>");
	  pchar += 6; /* = strlen("<port>") */
	  pLast = strchr(pchar, '<');
	  count = pLast - pchar;
	  strncpy(tmpBuf, pchar, count);
	  tmpBuf[count]='\0';
	  *port = (unsigned short int)atoi(tmpBuf);
	  break;
	}
	pLast++;
      }
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Get_data_sink_address_wsrf: proxy host = %s\n"
	      "                                   proxy port = %d\n",
	      hostname, *port);
#endif /* REG_DEBUG */

    }
  }

  /* So long as soap call did return something we return success - even
     if we didn't actually get a valid address.  This consistent with
     polling a GS for valid address - success is indicated by non-zero
     port no. */
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Record_checkpoint_set_impl(int ChkType, char* ChkTag, char* Path) {
  int    nfiles;
  int    i, j, status, len;
  int    count = 0;
  int    nbytes, bytes_left;
  char **filenames;
  char  *cp_data;
  char   node_data[REG_MAX_MSG_SIZE];
  char  *pchar;
  char  *pTag;
  time_t time_now;
  int    index;
  struct sws__RecordCheckpointResponse response;

  /* Get list of checkpoint files */

  index = IOdef_index_from_handle(&ChkTypes_table, ChkType);
  if(ChkTypes_table.io_def[index].buffer_bytes == 0){

    /* Calc. length of string - 'ls -1' and slashes add 9 chars so
       add a few more for safety.  Ask for 2*strlen(ChkTag) so that
       we can use the end of this buffer to hold the trimmed version
       of the tag. */
    len = strlen(Path) + 2*strlen(ChkTag) +
      strlen(Steer_lib_config.working_dir) + 20;

    if( !(pchar = (char *)malloc(len)) ){

      fprintf(stderr, "STEER: Record_checkpoint_set: malloc of %d bytes failed\n",
	      len);
      return REG_FAILURE;
    }

    /* Set our pointer to the 'spare' bit at the end of the buffer
       we've just malloc'd */
    pTag = &(pchar[len - strlen(ChkTag) - 1]);

    /* Trim off leading space... */
    len = strlen(ChkTag);
    j = -1;
    for(i=0; i<len; i++){

      if(ChkTag[i] != ' '){
	j = i;
	break;
      }
    }

    if(j == -1){
      fprintf(stderr, "STEER: Record_checkpoint_set: ChkTag is blank\n");
      return REG_FAILURE;
    }

    /* Copy tag until first blank space - i.e. tag must not contain any
       spaces. */
    for(i=j; i<len; i++){
      if(ChkTag[i] == ' ')break;
      pTag[count++] = ChkTag[i];
    }
    pTag[count] = '\0';

    sprintf(pchar, "%s/%s", Steer_lib_config.working_dir, Path);

    filenames = NULL;
    status = Get_file_list(pchar, 1, &pTag, &nfiles, &filenames);
    free(pchar);

    if( (status != REG_SUCCESS) || !nfiles){

      fprintf(stderr, "STEER: Record_checkpoint_set: failed to find checkpoint "
	      "files with tag >%s<\n", ChkTag);
      return REG_FAILURE;
    }
  }
  else{

    /* Temporarily store the path to the files in the node_data string
       'cos we don't use that until later */
    sprintf(node_data, "%s/%s/", Steer_lib_config.working_dir, Path);

    /* Filenames have been added by calls to Add_checkpoint_file */
    pchar = (char *)ChkTypes_table.io_def[index].buffer;

    nfiles = 0;
    while( (pchar = strchr(++pchar, ' ')) ){
      nfiles++;
    }
    if(nfiles > 0){
      filenames = (char **)malloc(nfiles * sizeof(char*));
      if(!filenames){
	fprintf(stderr, "STEER: Record_checkpoint_set: failed to malloc "
		"filenames array\n");
	return REG_FAILURE;
      }

      len = strlen(node_data); /* Get length of path */
      pchar = (char *)ChkTypes_table.io_def[index].buffer;
      for(i=0; i<nfiles; i++){
	pTag = strchr(pchar, ' ');
	filenames[i] = (char *)malloc((pTag - pchar) + 2 + len);
	if(!filenames[i]){
	  fprintf(stderr, "STEER: Record_checkpoint_set: malloc for filename "
		  "%d failed\n", i);
	  return REG_FAILURE;
	}
	strcpy(filenames[i], node_data); /* Put path at beginning of filename */
	strncat(filenames[i], pchar, (pTag-pchar));
	filenames[i][(pTag-pchar)+1+len] = '\0';
	pchar = ++pTag;
      }
    } /* nfiles > 0 */

    /* Reset contents of ChkType buffer ready for next checkpoint */
    memset(ChkTypes_table.io_def[index].buffer, '\0',
	   ChkTypes_table.io_def[index].buffer_max_bytes);
    ChkTypes_table.io_def[index].buffer_bytes = 0;
  }

  /* Construct checkpoint meta-data */
  cp_data = Steer_lib_config.scratch_buffer;
  pchar = cp_data;
  bytes_left = REG_SCRATCH_BUFFER_SIZE;
  nbytes = snprintf(pchar, bytes_left, "<Checkpoint_data application=\"%s\">\n"
		    "<Chk_type>%d</Chk_type>\n"
		    "<Chk_UID>%s</Chk_UID>\n"
		    "<Files location=\"%s\">\n",
		    ReG_AppName, ChkType, ChkTag, ReG_Hostname);
  pchar += nbytes;
  bytes_left -= nbytes;

  for(i=0; i<nfiles; i++){

    nbytes = snprintf(pchar, bytes_left, "  <file type=\"gsiftp-URL\">"
		      "gsiftp://%s%s</file>\n",
		      ReG_Hostname, filenames[i]);

    /* Check for truncation */
    if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
      fprintf(stderr, "STEER: Record_checkpoint_set: data exceeds %d chars\n",
	      REG_SCRATCH_BUFFER_SIZE);
      return REG_FAILURE;
    }
    pchar += nbytes;
    bytes_left -= nbytes;
    free(filenames[i]);
  }
  free(filenames);
  filenames = NULL;

  nbytes = snprintf(pchar, bytes_left, "</Files>\n</Checkpoint_data>\n");
  pchar += nbytes;
  bytes_left -= nbytes;

  /* Store the values of all registered parameters at this point (so
     long as they're not internal to the library) */
  memset(node_data, '\0', REG_MAX_MSG_SIZE);
  pchar = node_data;
  bytes_left = REG_MAX_MSG_SIZE;
  nbytes = snprintf(pchar, bytes_left, "<Checkpoint_node_data>\n");

  pchar += nbytes;
  bytes_left -= nbytes;

  for(i = 0; i<Params_table.max_entries; i++){

    if(Params_table.param[i].handle == REG_PARAM_HANDLE_NOTSET ||
       Params_table.param[i].is_internal == REG_TRUE){

      /* Time stamp is a special case - is internal but we do want
	 it for checkpoint records */
      if(Params_table.param[i].handle != REG_TIMESTAMP_HANDLE){
	continue;
      }

      /* Get timestamp */
      if( (int)(time_now = time(NULL)) != -1){
	strcpy(Params_table.param[i].value, ctime(&time_now));
	/* Remove new-line character */
	Params_table.param[i].value[strlen(pchar)-1] = '\0';
      }
      else{
	strcpy(Params_table.param[i].value, "");
      }
    }

    /* Don't include raw binary parameters in the log */
    if(Params_table.param[i].type == REG_BIN)continue;

    /* Update value associated with pointer */
    Get_ptr_value(&(Params_table.param[i]));

    nbytes = snprintf(pchar, bytes_left,
		      "<Param>\n"
		      "<Handle>%d</Handle>\n"
		      "<Label>%s</Label>\n"
		      "<Value>%s</Value>\n</Param>\n",
		      Params_table.param[i].handle,
		      Params_table.param[i].label,
		      Params_table.param[i].value);

    /* Check for truncation */
    if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
      fprintf(stderr, "STEER: Record_checkpoint_set: node metadata "
	      "exceeds %d chars\n", REG_MAX_MSG_SIZE);
      return REG_FAILURE;
    }

    pchar += nbytes;
    bytes_left -= nbytes;
  }

  nbytes = snprintf(pchar, bytes_left, "</Checkpoint_node_data>\n");
  if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
    fprintf(stderr, "STEER: Record_checkpoint_set: node metadata "
	    "exceeds %d chars\n", REG_MAX_MSG_SIZE);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Record_checkpoint_set: node meta data >>%s<<\n",
	  node_data);
  fprintf(stderr, "STEER: Record_checkpoint_set: cp_data >>%s<<\n",
	  cp_data);
#endif

  /* Record checkpoint */

  create_WSRF_header(appside_SGS_info.soap,
                     appside_SGS_info.address,
		     appside_SGS_info.username,
		     appside_SGS_info.passwd);

  response._RecordCheckpointReturn = NULL;
  if(soap_call_sws__RecordCheckpoint(appside_SGS_info.soap,
				      appside_SGS_info.address,
				      "", cp_data, node_data,
				      &response)){
    fprintf(stderr, "STEER: Record_checkpoint_set_wsrf: soap call failed:\n");
    soap_print_fault(appside_SGS_info.soap, stderr);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  if(response._RecordCheckpointReturn){
    fprintf(stderr, "STEER: Record_checkpoint_set_wsrf: "
	    "RecordCheckpoint returned: >>%s<<\n",
	    response._RecordCheckpointReturn);
  }
  else{
    fprintf(stderr, "STEER: Record_checkpoint_set_wsrf: RecordCheckpoint "
	    "returned null\n");
  }
#endif

  return REG_SUCCESS;
}

/*---------------- Steerside methods --------------------*/

extern Sim_table_type Sim_table;
extern Steerer_config_table_type Steer_config;
SGS_info_table_type steerer_SGS_info_table;

int Initialize_steerside_transport() {
  strncpy(Steer_lib_config.Steering_transport_string, "WSRF", 5);

  if(init_ssl_random_seq() == REG_SUCCESS){
    Steer_config.ossl_rand_available = REG_TRUE;
  }
  else{
    Steer_config.ossl_rand_available = REG_FALSE;
  }

  return SGS_info_table_init(&steerer_SGS_info_table,
			     REG_MAX_NUM_STEERED_SIM);
}

/*-------------------------------------------------------*/

int Finalize_steerside_transport() {
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Sim_attach_impl(int index, char* SimID) {
  struct sws__AttachResponse response;
  Sim_entry_type* sim;
  SGS_info_type* SGS_info;
  int i;

  sim = &(Sim_table.sim[index]);
  SGS_info = &(steerer_SGS_info_table.SGS_info[index]);
  SGS_info->lastModTime = 0;

  /* malloc memory for soap struct for this connection and then
     initialise it */
  SGS_info->soap = (struct soap*)malloc(sizeof(struct soap));
  if(!(SGS_info->soap)) {

    fprintf(stderr, "STEER: Sim_attach: failed to malloc memory for "
	    "soap struct\n");
    return REG_FAILURE;
  }
  /* Use this form to turn-on keep-alive for both incoming and outgoing
     http connections */
  soap_init2(SGS_info->soap, SOAP_IO_KEEPALIVE, SOAP_IO_KEEPALIVE);

#ifdef REG_DEBUG
  /* ARPDBG Ptr to a handler so we can see which tags we're ignoring */
  SGS_info->soap->fignore = soap_mismatch_handler;
#endif

  /* If address of SWS begins with 'https' then initialize SSL context */
  if(strstr(SimID, "https") == SimID) {
    if(init_ssl_context(SGS_info->soap,
			REG_TRUE, /* Authenticate SWS */
			NULL,/*char *certKeyPemFile,*/
			NULL, /* char *passphrase,*/
			Steer_config.caCertsPath) == REG_FAILURE) {

      fprintf(stderr, "STEER: ERROR: Sim_attach_wsrf: call to initialize "
	      "soap SSL context failed\n");
      return REG_FAILURE;
    }
  }

  if(create_WSRF_header(SGS_info->soap,
			SimID,
			SGS_info->username,
			SGS_info->passwd) != REG_SUCCESS) {
    return REG_FAILURE;
  }

  if(soap_call_sws__Attach(SGS_info->soap, SimID, "", NULL,
			   &response) != SOAP_OK) {
    soap_print_fault((SGS_info->soap), stderr);
    Finalize_connection_impl(index);
    return REG_FAILURE;
  }

  /* That worked OK so store address of SWS */
  sprintf(SGS_info->address, SimID);

  for(i=0; i<response.ReG_USCOREsteer_USCOREmessage.Supported_USCOREcommands.__size; i++) {
    sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id =
      response.ReG_USCOREsteer_USCOREmessage.Supported_USCOREcommands.__ptr[i].Cmd_USCOREid;

    /* ARPDBG - may need to add cmd parameters here too */
    Increment_cmd_registered(&(sim->Cmds_table));
  }

  if(!(response.ReG_USCOREsteer_USCOREmessage.Supported_USCOREcommands.__size)) {
    /* Cannot have no supported commands - detach must be supported so this
       is an error */
    fprintf(stderr, "STEER: ERROR: Attach: no supported commands returned\n");
    Finalize_connection_impl(index);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Sim_attach_security_impl(const int index,
			     const struct reg_security_info* sec) {

  strncpy(steerer_SGS_info_table.SGS_info[index].username,
	  sec->userDN, REG_MAX_STRING_LENGTH);
  strncpy(steerer_SGS_info_table.SGS_info[index].passwd,
	  sec->passphrase, REG_MAX_STRING_LENGTH);

  if(sec->caCertsPath[0]){
    strncpy(Steer_config.caCertsPath, sec->caCertsPath,
	    REG_MAX_STRING_LENGTH);
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Finalize_connection_impl(int index) {
  Sim_entry_type *sim;
  SGS_info_type* SGS_info;

  sim = &(Sim_table.sim[index]);
  SGS_info = &(steerer_SGS_info_table.SGS_info[index]);

  if(sim->detached == REG_FALSE) {
    if(Send_detach_msg_impl(index) == REG_SUCCESS) {
      sim->detached = REG_TRUE;
    }
  }

  /* Remove temporary data and deserialized data except
     class instances */
  soap_end(SGS_info->soap);
  /* Reset: close master/slave sockets and remove callbacks */
  soap_done(SGS_info->soap);

  free(SGS_info->soap);
  SGS_info->soap = NULL;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

struct msg_struct* Get_status_msg_impl(int index, int ignore) {
  char *pRPDoc;
  char buf[REG_MAX_MSG_SIZE];
  long int modTime;
  struct msg_struct *msg = NULL;
  Sim_entry_type *sim;
  SGS_info_type* SGS_info;

  sim = &(Sim_table.sim[index]);
  SGS_info = &(steerer_SGS_info_table.SGS_info[index]);

  /* If we have a backlog of messages then return the next one */
  if((msg = get_next_stored_msg(sim))) {
    return msg;
  }

  if(get_resource_property_doc(SGS_info->soap,
			       SGS_info->address,
			       SGS_info->username,
			       SGS_info->passwd,
			       &pRPDoc) != REG_SUCCESS) {

    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }

  /* Check for changes to RP first (a.k.a. notifications) */
  if(Extract_resource_property(pRPDoc, strlen(pRPDoc),
			       "sws:lastModifiedTime",
			       buf, REG_MAX_MSG_SIZE) != REG_SUCCESS){
    fprintf(stderr, "STEER: Get_status_msg_wsrf: failed to get "
	    "lastModifiedTime from ResourceProperty document\n");
    return NULL;
  }

  modTime = atoi(buf);

  if(modTime != SGS_info->lastModTime){
    SGS_info->lastModTime=modTime;

    /* Parse the whole doc; messages are stored in the Msg_store struct
       associated with the sim entry */
    Parse_xml_buf(pRPDoc, strlen(pRPDoc), NULL, sim);
    return get_next_stored_msg(sim);
  }
  else{
    /* Just get the latest status message from the app */
    if(Extract_resource_property(pRPDoc, strlen(pRPDoc),
				 "sws:latestStatusMsg",
				 buf, REG_MAX_MSG_SIZE) == REG_SUCCESS){

      msg = New_msg_struct();
      if(Parse_xml_buf(buf, strlen(buf), msg, sim) != REG_SUCCESS){

	Delete_msg_struct(&msg);
	return NULL;
      }
    }
  }

  return msg;
}

/*-------------------------------------------------------*/

int Send_control_msg_impl(int index, char *buf) {
  char   inputBuf[REG_MAX_MSG_SIZE + 32];
  SGS_info_type* SGS_info;

  SGS_info = &(steerer_SGS_info_table.SGS_info[index]);

  snprintf(inputBuf, REG_MAX_MSG_SIZE + 32,
	   "<controlMsg>%s</controlMsg>", buf);

  return set_resource_property(SGS_info->soap,
			       SGS_info->address,
			       SGS_info->username,
			       SGS_info->passwd,
			       inputBuf);
}

/*-------------------------------------------------------*/

int Send_detach_msg_impl(int index) {
  SGS_info_type* SGS_info;

  SGS_info = &(steerer_SGS_info_table.SGS_info[index]);

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Send_detach_msg: calling Detach...\n");
#endif

  if(create_WSRF_header(SGS_info->soap,
			SGS_info->address,
			SGS_info->username,
			SGS_info->passwd) != REG_SUCCESS) {
    return REG_FAILURE;
  }

  if(soap_call_sws__Detach(SGS_info->soap, SGS_info->address,
			   "", NULL, NULL )) {

    fprintf(stderr, "STEER: Send_detach_msg_wsrf: Detach failed:\n");
    soap_print_fault(SGS_info->soap, stderr);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Get_param_log_impl(int sim_index, int handle) {
  struct sws__GetParamLogResponse response;
  int    param_index;
  int    log_index;
  char  *ptr1;
  int    dum_int;
  long   dum_long;
  float  dum_float;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif
  Sim_entry_type *sim;
  SGS_info_type* SGS_info;

  sim = &(Sim_table.sim[sim_index]);
  SGS_info = &(steerer_SGS_info_table.SGS_info[sim_index]);
  response.LogValues = NULL;

  if((param_index=Param_index_from_handle(&(sim->Params_table),
				     handle)) == -1) {
      fprintf(stderr, "STEER: Get_param_log: failed to match param handle\n");
      fprintf(stderr, "                      handle = %d\n", handle);

      return REG_FAILURE;
  }

  if(!sim->Params_table.param[param_index].log) {
    if(Realloc_param_log(&(sim->Params_table.param[param_index])) !=
       REG_SUCCESS) {
      return REG_MEM_FAIL;
    }
  }

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif
  if(SGS_info->passwd[0]){
    if(create_WSRF_header(SGS_info->soap,
			  SGS_info->address,
			  SGS_info->username,
			  SGS_info->passwd) != REG_SUCCESS){
      return REG_FAILURE;
    }
  }
  if(soap_call_sws__GetParamLog(SGS_info->soap, SGS_info->address,
				"", (xsd__int)handle, &response )){
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "STEER: TIMING: soap_call_sws__GetParamLog "
	  "took %f seconds\n", (time1-time0));
#endif

    fprintf(stderr, "STEER: Get_param_log_wsrf: failed for handle %d:\n",
	    handle);
    soap_print_fault(SGS_info->soap, stderr);

    return REG_FAILURE;
  }

  if( !(response.LogValues) ){
    fprintf(stderr, "STEER: Get_param_log_wsrf: no log entries returned\n");
    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Get_param_log_wsrf: got log from SWS >>%s<<\n",
	  (char*)response.LogValues);
#endif

  ptr1 = (char*)response.LogValues;
  log_index = sim->Params_table.param[param_index].log_index;

  switch(sim->Params_table.param[param_index].type){

  case REG_INT:

    while(1) {

      /* Parse space-delimited list of parameter values */
      sscanf(ptr1, "%d ", &dum_int);
      sim->Params_table.param[param_index].log[log_index++] = (double)dum_int;

      if(!(ptr1 = strchr(ptr1, ' ')))break;
      if(*(++ptr1) == '\0')break;

      if(log_index >= sim->Params_table.param[param_index].log_size){
	Realloc_param_log(&(sim->Params_table.param[param_index]));
      }
    }
    sim->Params_table.param[param_index].log_index = log_index;
    break;

  case REG_LONG:

    while(1) {

      /* Parse space-delimited list of parameter values */
      sscanf(ptr1, "%ld ", &dum_long);
      sim->Params_table.param[param_index].log[log_index++] = (double)dum_long;

      if(!(ptr1 = strchr(ptr1, ' ')))break;
      if(*(++ptr1) == '\0')break;

      if(log_index >= sim->Params_table.param[param_index].log_size){
	Realloc_param_log(&(sim->Params_table.param[param_index]));
      }
    }
    sim->Params_table.param[param_index].log_index = log_index;
    break;

  case REG_FLOAT:

    while(1) {

      sscanf(ptr1, "%f", &dum_float);
      sim->Params_table.param[param_index].log[log_index++] = (double)dum_float;

      if(!(ptr1 = strchr(ptr1, ' ')))break;
      if(*(++ptr1) == '\0')break;

      if(log_index >= sim->Params_table.param[param_index].log_size){
	Realloc_param_log(&(sim->Params_table.param[param_index]));
      }
    }
    sim->Params_table.param[param_index].log_index = log_index;
    break;

  case REG_DBL:

    while(1) {

      sscanf(ptr1, "%lg", &(sim->Params_table.param[param_index].log[log_index++]) );

      if(!(ptr1 = strchr(ptr1, ' ')))break;
      if(*(++ptr1) == '\0')break;

      if(log_index >= sim->Params_table.param[param_index].log_size){
	Realloc_param_log(&(sim->Params_table.param[param_index]));
      }
    }
    sim->Params_table.param[param_index].log_index = log_index;
    break;

  case REG_CHAR:
    /* This not implemented yet */
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Get_param_log: logging of char params not "
	    "implemented!\n");
#endif
    break;

  default:
    break;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Get_registry_entries_impl(const char* registryEPR,
			      const struct reg_security_info* sec,
			      struct registry_contents* contents) {

  struct wsrp__GetMultipleResourcePropertiesRequest in;
  struct soap soap;
  int    status = REG_SUCCESS;
  char  *out;
  int    i;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif
  contents->numEntries = 0;
  contents->entries = NULL;

  soap_init(&soap);

  /* regServiceGroup can use SSL for authentication */
  /* If address of SWS begins with 'https' then initialize SSL context */
  if((strstr(registryEPR, "https") == registryEPR)) {
    if(init_ssl_context(&soap,
			REG_TRUE, /* Authenticate SWS */
			sec->myKeyCertFile,
			sec->passphrase,
			sec->caCertsPath) == REG_FAILURE) {
      fprintf(stderr, "Get_registry_entries: call to initialize "
	      "soap SSL context failed\n");
      return REG_FAILURE;
    }
    status = create_WSRF_header(&soap, registryEPR, NULL, NULL);
  }
  else {
    /* Otherwise we just use WSSE */
    status = create_WSRF_header(&soap, registryEPR,
				sec->userDN, sec->passphrase);
  }

  if(status != REG_SUCCESS) {
    return REG_FAILURE;
  }

  in.__size = 1;
  in.__ptr = (struct wsrp__ResourcePropertyStruct*) malloc(in.__size *
							   sizeof(struct wsrp__ResourcePropertyStruct));
  if(!in.__ptr) {
    fprintf(stderr, "ERROR: Get_registry_entries: malloc failed\n");
    return REG_MEM_FAIL;
  }

  for(i=0; i<in.__size;i++) {
    in.__ptr[i].ResourceProperty = (char*) malloc(16 * sizeof(char));
    if(!(in.__ptr[i].ResourceProperty))
      return REG_MEM_FAIL;
  }
  /* Entries in a ServiceGroup are held in the 'Entry' ResourceProperty */
  sprintf(in.__ptr[0].ResourceProperty, "Entry");

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

  if(soap_call_wsrp__GetMultipleResourceProperties(&soap,
						   registryEPR,
						   "", in,
						   &out) != SOAP_OK) {
    soap_print_fault(&soap, stderr);
    free(in.__ptr[0].ResourceProperty);
    soap_end(&soap);
    soap_done(&soap);
    return REG_FAILURE;
  }

  free(in.__ptr[0].ResourceProperty);
  free(in.__ptr);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "STEERUtils: TIMING: soap_call_wsrp__GetMultipleResourceProperties "
	  "took %f seconds\n", (time1-time0));
#endif

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEERUtils: Get_registry_entries_wsrf: "
	  "Get_resource_property for Entry returned >>%s<<\n\n", out);
#endif
  if(strlen(out) > 0){
    status = Parse_registry_entries(out,
				    strlen(out),
				    contents);
  }

  soap_end(&soap);
  soap_done(&soap);

  return status;
}

/*----------------- Internal methods --------------------*/

int create_WSRF_header(struct soap *aSoap,
		       const  char *epr,
		       const  char *username,
		       const  char *passwd) {

  int           bytesLeft, nbytes;
  int           i, len;
  int           status;
#define MAX_LEN 1024
  unsigned char randBuf[MAX_LEN];
  char         *pBuf;
  char          buf[MAX_LEN];
  char          digest[SHA_DIGEST_LENGTH];
  char         *pBase64Buf = NULL;
  char         *timePtr;

  /* alloc new header for WS-RF */
  aSoap->header = soap_malloc(aSoap, sizeof(struct SOAP_ENV__Header));
  if(!(aSoap->header)){
    fprintf(stderr,
	    "STEER: create_WSRF_header: Failed to malloc space for header\n");
    return REG_FAILURE;
  }

  aSoap->header->wsa__To = (char *)soap_malloc(aSoap, strlen(epr)+1);
  if(!(aSoap->header->wsa__To)){
    fprintf(stderr, "STEER: create_WSRF_header: Failed to malloc space "
	    "for header wsa:To element\n");
    return REG_FAILURE;
  }

  strcpy(aSoap->header->wsa__To, epr);

  if(!username || !(username[0])){
#ifdef REG_DEBUG
    fprintf(stderr,
	    "STEER: create_WSRF_header: not adding security to header\n");
#endif /* REG_DEBUG */
    aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Username = NULL;
    aSoap->header->wsse__Security.wsse__UsernameToken.wsu__Created = NULL;
    aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.Type = NULL;
    aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.__item = NULL;
    aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Nonce = NULL;
    return REG_SUCCESS;
  }

  /* Set up the username element of the header irrespective of whether
     or not the library has been built with openSSL support */
  aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Username =
                                       (char *)soap_malloc(aSoap, 128);

  if(!(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Username)){
    fprintf(stderr, "STEER: create_WSRF_header: Failed to malloc space "
	    "for Username element\n");
    return REG_FAILURE;
  }
  snprintf(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Username,
	   128, username);

  aSoap->header->wsse__Security.wsse__UsernameToken.wsu__Created =
                                       (char *)soap_malloc(aSoap, 128);
  aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.Type =
                                       (char *)soap_malloc(aSoap, 128);

  if( !(aSoap->header->wsse__Security.wsse__UsernameToken.wsu__Created) ||
      !(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.Type) ){
    fprintf(stderr, "STEER: create_WSRF_header: Failed to malloc space "
	    "for header elements\n");
    return REG_FAILURE;
  }

  if(ReG_ssl_random_initialized == REG_FALSE){
    if(init_ssl_random_seq() != REG_SUCCESS){
      fprintf(stderr, "STEER: create_WSRF_header: Failed to "
	      "initialize SSL random number generator\n");
      return REG_FAILURE;
    }
  }

  /* This call requires that init_ssl_random_seq() has been called previously */
  status = RAND_pseudo_bytes(randBuf, 16);
  if(status == 0){
    fprintf(stderr, "STEER: WARNING: create_WSRF_header: Sequence is not "
	    "cryptographically strong\n");
  }
  else if(status == -1){
    fprintf(stderr, "STEER: ERROR: create_WSRF_header: RAND_pseudo_bytes "
	    "is not supported\n");
    return REG_FAILURE;
  }

  /* Base64-encode this random sequence to make our nonce a nice
     ASCII string (XML friendly) */
  Base64_encode((char*) randBuf, 16, &pBase64Buf, (unsigned int*) &len);

  aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Nonce =
                                       (char *)soap_malloc(aSoap, len+1);
  if( !(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Nonce) ){
    fprintf(stderr,
	    "STEER: create_WSRF_header: Failed to malloc space for nonce\n");
    return REG_FAILURE;
  }
  strncpy(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Nonce,
	  pBase64Buf, len);
  aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Nonce[len] = '\0';

  timePtr = Get_current_time_string(); /* Steer lib */
  snprintf(aSoap->header->wsse__Security.wsse__UsernameToken.wsu__Created,
	   128, timePtr);
  snprintf(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.Type,
	   128, "PasswordDigest");

  /* Password_digest = Base64(SHA-1(nonce + created + password)) */
  bytesLeft = MAX_LEN;
  pBuf = buf;
  /* Nonce */
  nbytes = snprintf(pBuf, bytesLeft,
		    aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Nonce);
  bytesLeft -= nbytes; pBuf += nbytes;
  /* Created */
  nbytes = snprintf(pBuf, bytesLeft, timePtr);
  bytesLeft -= nbytes; pBuf += nbytes;
  /* Password */
  nbytes = snprintf(pBuf, bytesLeft, passwd);
  bytesLeft -= nbytes; pBuf += nbytes;

  SHA1((unsigned char*) buf, (MAX_LEN-bytesLeft), (unsigned char*) digest); /* openssl call */

  free(pBase64Buf); len = 0; pBase64Buf=NULL;
  Base64_encode(digest, SHA_DIGEST_LENGTH, &pBase64Buf, (unsigned int*) &len); /* Steer lib */
  /* Strip padding characters from end because perl doesn't add them from
     the sha1_base64 function */
  i = len-1;
  while((i > -1) && (pBase64Buf[i] == '=')){
    pBase64Buf[i--] = '\0';
  }

  /* +1 allows for null terminator (which Base64_encode does not include) */
  aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.__item =
                                       (char *)soap_malloc(aSoap, len+1);
  if( !(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.__item) ){
    fprintf(stderr, "STEER: create_WSRF_header: Failed to malloc "
	    "space for Password\n");
    return REG_FAILURE;
  }
  strncpy(aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.__item,
	  pBase64Buf, len);
  aSoap->header->wsse__Security.wsse__UsernameToken.wsse__Password.__item[len] = '\0';

  free(pBase64Buf);
  pBase64Buf = NULL;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int destroy_WSRP(const char* epr,
		 const struct reg_security_info* sec) {
  struct wsrp__DestroyResponse out;
  struct soap soap;
  int    return_status = REG_SUCCESS;
  if(epr) {
    soap_init(&soap);
    /* Something to do with the XML type */
    soap.encodingStyle = NULL;

    return_status = create_WSRF_header(&soap, epr, NULL, NULL);

    if(return_status != REG_SUCCESS)
      return REG_FAILURE;

    /* If we're using https then set up the context */
    if((strstr(epr, "https") == epr)) {
      if(init_ssl_context(&soap,
			  REG_TRUE, /* Authenticate SWS */
			  NULL,/*char *certKeyPemFile,*/
			  NULL, /* char *passphrase,*/
			  sec->caCertsPath) == REG_FAILURE) {

	fprintf(stderr, "ERROR: Destroy_WSRP: call to initialize soap "
		"SSL context failed\n");
	return REG_FAILURE;
      }
    }

    if(soap_call_wsrp__Destroy(&soap, epr, NULL, NULL, &out) != SOAP_OK) {
      fprintf(stderr, "destroy_WSRP: call to Destroy on %s failed:\n   ", epr);
      soap_print_fault(&soap, stderr);
      return_status = REG_FAILURE;
    }

    soap_end(&soap); /* dealloc deserialized data */
    soap_done(&soap); /* cleanup and detach soap struct */
  }
  return return_status;
}

/*-------------------------------------------------------*/

int get_resource_property (struct soap *soapStruct,
                           const char  *epr,
			   const char  *username,
			   const char  *passwd,
			   const char  *name,
			   char       **pRP) {
  char  *out;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  *pRP = NULL;
  if(create_WSRF_header(soapStruct, epr, username, passwd) != REG_SUCCESS) {
    return REG_FAILURE;
  }

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

  if(soap_call___wsrp__GetResourceProperty(soapStruct, epr,
					 "", (char*)name,
					 &out) != SOAP_OK) {
    soap_print_fault(soapStruct, stderr);
    return REG_FAILURE;
  }
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
#ifdef REG_DEBUG
  fprintf(stderr, "STEER: TIMING: soap_call___wsrp__GetResourceProperty"
	  " took %f seconds\n", (float)(time1-time0));
#endif
#endif
#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: get_resource_property for %s returned >>%s<<\n",
	  name, out);
#endif

  *pRP = out;
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int get_resource_property_doc(struct soap *soapStruct,
			      const char  *epr,
			      const char  *username,
			      const char  *passwd,
			      char       **pDoc) {
  char *out;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif

 /* Free-up dynamically-allocated memory regularly because the RP
    doc can be big */
  soap_end(soapStruct);

  if(create_WSRF_header(soapStruct, epr, username, passwd) != REG_SUCCESS)
    return REG_FAILURE;

  *pDoc = NULL;

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

  if(soap_call_wsrp__GetResourcePropertyDocument(soapStruct,
						 (char *)epr,
						 "", NULL,
						 &out) != SOAP_OK) {
    soap_print_fault(soapStruct, stderr);
    return REG_FAILURE;
  }

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
#ifdef REG_DEBUG
  fprintf(stderr, "STEER: TIMING: soap_call_wsrp__GetResourcePropertyDocument "
	  "took %f seconds\n", (time1-time0));
#endif
#endif

  if(!(*pDoc = out)) {
    fprintf(stderr, "STEER: ERROR: Get_resource_property_doc: gSoap call "
	    "successful but have NULL pointer to document!\n");
    return REG_FAILURE;
  }
  else{
    return REG_SUCCESS;
  }
}

/*-------------------------------------------------------*/

int set_resource_property (struct soap *soapStruct,
                           const char  *epr,
			   const char  *username,
			   const char  *passwd,
			   char        *input) {
  struct wsrp__SetResourcePropertiesResponse out;

  if(create_WSRF_header(soapStruct, epr, username, passwd) != REG_SUCCESS)
    return REG_FAILURE;

  if(soap_call_wsrp__SetResourceProperties(soapStruct, epr,
					   "", input, &out) != SOAP_OK) {
    fprintf(stderr, "STEER: set_resource_property: failed to set RP:\n");
    soap_print_fault(soapStruct, stderr);

    if(soapStruct->fault && soapStruct->fault->detail &&
       soapStruct->fault->detail->__any) {
	fprintf(stderr, "STEER: set_resource_property: Soap error detail"
		" any = %s\n", soapStruct->fault->detail->__any);
	if(strstr(soapStruct->fault->detail->__any, "deadlock")) {
	  /* If we timed-out because of a deadlock situation (possible in
	     coupled models) then return appropriate code */
	  return REG_TIMED_OUT;
	}
      }

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

struct msg_struct* get_next_stored_msg(Sim_entry_type* sim) {
  struct msg_struct *msg = NULL;
  struct msg_store_struct *msgStorePtr = &Msg_store;
  struct msg_store_struct *msgStoreTailPtr = Msg_store_tail;
  struct msg_store_struct *tmp;

  if(sim) {
    msgStorePtr = &(sim->Msg_store);
    msgStoreTailPtr = sim->Msg_store_tail;
  }

  if(msgStorePtr->msg) {

    /* Take a copy of the pointer to the oldest stored message */
    msg = msgStorePtr->msg;

    /* Then shift pointers to point at the next one, if any */
    if((tmp = msgStorePtr->next)) {
      msgStorePtr->msg = tmp->msg;
      msgStorePtr->next = tmp->next;
      if(tmp == msgStoreTailPtr) {
	msgStoreTailPtr = msgStorePtr;
      }
      /* Delete the struct we've just copied ptrs from */
      free(tmp);
      tmp = NULL;
    }
    else {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Get_next_stored_msg: hit end of msg store\n");
#endif
      msgStorePtr->msg = NULL;
      msgStorePtr->next = NULL;
      /* Make sure the ptr to the tail of the list is
	 reset too */
      msgStoreTailPtr = msgStorePtr;
    }
  }
#ifdef REG_DEBUG
  else {
    fprintf(stderr, "STEER: Get_next_stored_msg: no msg to retrieve\n");
  }
#endif

  if(sim) {
    sim->Msg_store_tail = msgStoreTailPtr;
  }
  else {
    Msg_store_tail = msgStoreTailPtr;
  }
  return msg;
}

/*-------------------------------------------------------*/

int SGS_info_table_init(SGS_info_table_type* table,
			const int max_entries) {
  int i;

  table->max_entries = max_entries;
  table->num_used = 0;
  table->SGS_info = (SGS_info_type*)
    malloc(max_entries * sizeof(SGS_info_type));

  if(table->SGS_info == NULL) {
    fprintf(stderr, "STEER: SGS_info_table_init: failed to allocate memory "
	    "for SGS info table\n");
    return REG_FAILURE;
  }

  for(i = 0; i < max_entries; i++) {
    table->SGS_info[i].active = REG_FALSE;
    table->SGS_info[i].sde_count = 0;
    table->SGS_info[i].sde_index = 0;
    table->SGS_info[i].username[0] = '\0';
    table->SGS_info[i].passwd[0] = '\0';
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int soap_mismatch_handler(struct soap* soap, const char* tag) {
  printf("soap_mismatch_handler: tag = %s\n", tag);
  /*return SOAP_TAG_MISMATCH;	 every tag must be handled */
  return SOAP_OK;	/* We're just curious */
}

/*-------------------------------------------------------*/

int init_ssl_random_seq() {
  struct stat stbuf;
  char randBuf[REG_MAX_STRING_LENGTH];

  if(ReG_ssl_random_initialized == REG_TRUE)
    return REG_SUCCESS;

  if(!RAND_file_name(randBuf, (size_t)REG_MAX_STRING_LENGTH)){
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: WARNING: Init_random: RAND_file_name failed to "
	    "return name of file for use in initializing randome "
	    "sequence - trying /dev/urandom\n");
#endif
    sprintf(randBuf, "/dev/urandom");
  }

  if(stat(randBuf, &stbuf) != -1){
  }
  else if(stat("/dev/urandom", &stbuf) != -1){
    sprintf(randBuf, "/dev/urandom");
  }
  else if(stat("/dev/random", &stbuf) != -1){
    sprintf(randBuf, "/dev/random");
  }
  else{
    fprintf(stderr, "STEER: Init_random: %s, /dev/urandom and /dev/random "
	    "do not exist on this system - cannot initalize random "
	    "sequence\n", randBuf);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Init_random: seed file is %s\n", randBuf);
#endif

  /* Use contents of randBuf file to initialise sequence of pseudo
     random numbers from OpenSSL */
  if(!RAND_load_file(randBuf, 16)){
    fprintf(stderr, "STEER: Init_random: Failed to initialize pseudo-random "
	    "number sequence from %s\n", randBuf);
    return REG_FAILURE;
  }

  ReG_ssl_random_initialized = REG_TRUE;
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int init_ssl_context(struct soap *aSoap,
		     const int  authenticateSWS,
		     const char *certKeyPemFile,
		     const char *passphrase,
		     const char *caCertPath) {

  struct stat stbuf;
  int soap_ssl_flag;

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: init_ssl_context arguments:\n"
	          " - authenticateSWS = %d\n"
                  " - certKeyPemFile = >>%s<<\n"
	          " - passphrase = >>%s<<\n"
                  " - caCertPath = >>%s<<\n",
	  authenticateSWS, certKeyPemFile, passphrase, caCertPath);
#endif

  if(!aSoap){
    fprintf(stderr, "STEER: init_ssl_context: pointer to soap "
	    "struct is NULL\n");
    return REG_FAILURE;
  }

  if((authenticateSWS == REG_TRUE) && !caCertPath){
    fprintf(stderr, "STEER: init_ssl_context: CA certificates "
	    "directory MUST be specified when SWS authentication is on\n");
    return REG_FAILURE;
  }
  else if( caCertPath && (stat(caCertPath, &stbuf) == -1) ){
    fprintf(stderr, "STEER: init_ssl_context: CA certificates "
	    "directory >>%s<< is not valid\n", caCertPath);
    if(errno == ENOENT){
      fprintf(stderr, "    Error from stat = ENOENT\n");
    }
    else if(errno == ENOTDIR){
      fprintf(stderr, "    Error from stat = ENOTDIR\n");
    }
    else if(errno == ELOOP){
      fprintf(stderr, "    Error from stat = ELOOP\n");
    }
    else if(errno == EFAULT){
      fprintf(stderr, "    Error from stat = EFAULT\n");
    }
    else if(errno == EACCES){
      fprintf(stderr, "    Error from stat = EACCES\n");
    }
    else if(errno == ENOMEM){
      fprintf(stderr, "    Error from stat = ENOMEM\n");
    }
    else if(errno == ENAMETOOLONG){
      fprintf(stderr, "    Error from stat = ENAMETOOLONG\n");
    }
    else{
      fprintf(stderr, "    Error from stat = UNKNOWN!\n");
    }

    return REG_FAILURE;
  }

  soap_ssl_flag = SOAP_SSL_NO_AUTHENTICATION;
  if(authenticateSWS == REG_TRUE){
    soap_ssl_flag = SOAP_SSL_DEFAULT;
  }

  if (soap_ssl_client_context(aSoap,
			      soap_ssl_flag,
			      /* user's cert. & key file */
			      certKeyPemFile,
			      /* Password to read key file */
			      passphrase,
			      /* Optional CA cert. file to store trusted
				 certificates (to verify server) */
			      NULL,
			      /* Optional path to directory containing
				 trusted CA certs (to verify server) */
			      caCertPath,
			      /* if randfile!=NULL: use a file with
				 random data to seed randomness */
			      NULL)){
    fprintf(stderr, "STEER: init_ssl_context: failed to initialize "
	    "gSoap ssl context:\n");
    soap_print_fault(aSoap, stderr);
    return REG_FAILURE;
  }
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/
