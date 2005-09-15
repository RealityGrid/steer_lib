/*----------------------------------------------------------------------------
  This header file contains defines routines and data structures for 
  parsing the XML steering-communication messages

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

  Authors........: Andrew Porter, Robert Haines

---------------------------------------------------------------------------*/

/** @file ReG_Steer_XML.h
    @brief Header file defining methods and structures for handling XML */

#ifndef __REG_STEER_XML_H__
#define __REG_STEER_XML_H__

#include "libxml/xmlmemory.h"
#include "libxml/parser.h"

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

#define REG_SUCCESS 0
#define REG_FAILURE 1

/*-----------------------------------------------------------------*/

/** Stucture for holding parsed elements of a parameter or param_def */
struct param_struct{

  xmlChar             *handle;
  xmlChar             *label;
  xmlChar             *value;
  xmlChar             *type;
  xmlChar             *steerable;
  xmlChar             *is_internal;
  xmlChar             *min_val;
  xmlChar             *max_val;
  struct param_struct *next;
};

/** Structure for holding parsed elements of a command */
struct cmd_struct{

  xmlChar             *id;
  xmlChar             *name;
  struct param_struct *first_param;
  struct param_struct *param;
  struct cmd_struct   *next;
};

/** Structure for holding parsed elements of a status msg */
struct status_struct{

  struct param_struct *first_param;
  struct param_struct *param;
  struct cmd_struct   *first_cmd;
  struct cmd_struct   *cmd;
};

/** Structure for holding parsed elements of IO- or Chk-Type */
struct io_struct{

  xmlChar  	   *label;
  xmlChar  	   *handle;
  xmlChar  	   *direction;
  xmlChar  	   *freq_handle;
  struct io_struct *next;
};

/** Structure for holding parsed elements of a control msg */
struct control_struct{

  xmlChar             *valid_after;
  struct param_struct *first_param;
  struct param_struct *param;
  struct cmd_struct   *first_cmd;
  struct cmd_struct   *cmd;
};

/** Structure for holding supported commands */
struct supp_cmd_struct{

  struct cmd_struct   *first_cmd;
  struct cmd_struct   *cmd;
};

/** Structure for holding IOType defs */
struct io_def_struct{
  
  struct io_struct *first_io;
  struct io_struct *io;
};

/** Strucutre for holding a single checkpoint log entry */
struct chk_log_entry_struct{

  xmlChar                 *chk_handle;
  xmlChar                 *chk_tag;
  struct param_struct     *first_param;
  struct param_struct     *param;
  struct chk_log_entry_struct *next;
};

/** Structure for single log entry */
struct log_entry_struct{

  xmlChar                 *key;
  struct param_struct     *first_param_log;
  struct param_struct     *param_log;
  struct chk_log_entry_struct *first_chk_log;
  struct chk_log_entry_struct *chk_log;
  struct log_entry_struct *next;
};

/** Structure for holding whole log */
struct log_struct{

  struct log_entry_struct *first_entry;
  struct log_entry_struct *entry;
};

/** Structure for holding a steering message */
struct msg_struct{

  int   	           msg_type;  
  xmlChar                 *msg_uid;
  struct status_struct    *status;
  struct control_struct   *control;
  struct supp_cmd_struct  *supp_cmd;
  struct io_def_struct    *io_def;
  struct io_def_struct    *chk_def;
  struct log_struct       *log;
};

/** Structure for storing multiple steering messages generated
    by parsing e.g. a ResourceProperties document */
struct msg_store_struct {
  struct msg_struct *msg;
  struct msg_store_struct *next;
};

/** Structure for storing the UIDs of previous
    messages so we don't act upon them again */
struct msg_uid_history_struct {
  unsigned int  uidStore[REG_UID_HISTORY_BUFFER_SIZE];
  unsigned int *uidStorePtr;
  unsigned int *maxPtr;
};


/** Definition of entry in main table holding data for connected simulations.  
    Contains five sub-tables - the first holding the commands that the 
    simulation supports, the second its registered parameters (both 
    steerable and monitored), the third its registered IO types, the fourth
    its registered Chk types and the fifth a log of checkpoints taken. */

typedef struct {

  int                  handle;

  /** For connection to applications using local file system - contains
      the location of the directory used to communicate with the sim. */
  char                 file_root[REG_MAX_STRING_LENGTH];

  /** File descriptors used to talk to (java) proxy for
      this simulation */
  int                  pipe_to_proxy;
  int                  pipe_from_proxy;

  /** Set to REG_TRUE once detach has been called - prevents us
      calling detach more than once on the SWS */
  int                  detached;

  /** Info on associated Grid service - for steering via SOAP */
  SGS_info_type SGS_info;

  /** Last status message received from this simulation - filled in
      Get_next_message and used by whichever Consume_... routine
      is called in response to the message type */
  struct msg_struct   *msg;

  /** Structure for holding multiple messages obtained by parsing
      SWS' ResourceProperties document */
  struct msg_store_struct  Msg_store;
  struct msg_store_struct *Msg_store_tail;

  /** Structure for holding the uid's of messages that we have
      previously consumed */
  struct msg_uid_history_struct Msg_uid_store;

  /** Table of registered commands for this sim */
  Supp_cmd_table_type  Cmds_table;

  /** Table of registered params for this sim */
  Param_table_type     Params_table;

  /** Table of registered IOTypes for this sim */
  IOdef_table_type     IOdef_table;

  /** Table of registered ChkTypes for this sim */
  IOdef_table_type     Chkdef_table;

  /** Table for logging checkpoint activity */
  Chk_log_type         Chk_log;

} Sim_entry_type;

/*-----------------------------------------------------------------*/

/** Parse the xml in the specified file
    @param filename name of the file to read and parse
    @param msg Pointer to message struct to hold results
    @param sim Pointer to Sim_entry struct or NULL
    @internal */
int Parse_xml_file(char* filename, struct msg_struct *msg,
		   Sim_entry_type *sim);
/** Parse the xml in the supplied buffer
    @param buf Pointer to buffer to parse
    @param size Size of the buffer to parse
    @param msg  Pointer to message struct to hold results
    @param sim Pointer to Sim_entry struct or NULL
    @internal */
int Parse_xml_buf(char* buf, int size, struct msg_struct *msg,
		  Sim_entry_type *sim);
/** Parse the DOM document and put the results in msg_struct
    @param sim Pointer to Sim_entry struct or NULL
    @internal */
int Parse_xml(xmlDocPtr doc, struct msg_struct *msg,
	      Sim_entry_type *sim);
/** Parse a steering message (ReG_steer_message) 
    @internal */
int parseSteerMessage(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		      struct msg_struct *msg, Sim_entry_type *sim);
/** Parse a Resource Properties document from a WSRF service
    @internal */
int parseResourceProperties(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
			    Sim_entry_type *sim);
/** Parse a Status message 
    @internal */
int parseStatus(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	        struct status_struct *status);
/** Parse a Control message 
    @internal */
int parseControl(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	         struct control_struct *ctrl);
/** Parse a Supported Commands message 
    @internal */
int parseSuppCmd(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		 struct supp_cmd_struct *supp_cmd);
/** Parse a Parameter element
    @internal */
int parseParam(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	       struct param_struct *param);
/** Parse a Command element
    @internal */
int parseCmd(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	     struct cmd_struct *cmd);
/** Parse a Checkpoint Type definition element
    @internal */
int parseChkTypeDef(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		    struct io_def_struct *chk_def);
/** Parse an IOType definition element
    @internal */
extern PREFIX int parseIOTypeDef(xmlDocPtr doc, xmlNsPtr ns, 
				 xmlNodePtr cur,
				 struct io_def_struct *io_def);
/** Parse an IOType/ChkType element
    @internal */
int parseIOType(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
                 struct io_struct *io);
/** Parse a Logging message
    @internal */
int parseLog(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	     struct log_struct *log);
/** Parse a Logging entry
    @internal */
int parseLogEntry(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		  struct log_entry_struct *log_entry);
/** Parse a Checkpoint Log entry
    @internal */
int parseChkLogEntry(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		     struct chk_log_entry_struct *log_entry);

struct msg_store_struct     *New_msg_store_struct();
extern PREFIX struct msg_struct           *New_msg_struct();
struct status_struct        *New_status_struct();
struct control_struct       *New_control_struct();
struct supp_cmd_struct      *New_supp_cmd_struct();
extern PREFIX struct io_def_struct        *New_io_def_struct();
struct io_struct            *New_io_struct();
struct param_struct         *New_param_struct();
struct cmd_struct           *New_cmd_struct();
struct chk_log_entry_struct *New_chk_log_entry_struct();
struct log_entry_struct     *New_log_entry_struct();
struct log_struct           *New_log_struct();

extern PREFIX void Delete_msg_struct(struct msg_struct **msgIn);
void Delete_status_struct(struct status_struct *status);
void Delete_control_struct(struct control_struct *ctrl);
void Delete_supp_cmd_struct(struct supp_cmd_struct *supp_cmd);
void Delete_param_struct(struct param_struct *param);
void Delete_cmd_struct(struct cmd_struct *cmd);
void Delete_io_def_struct(struct io_def_struct *io_def);
void Delete_io_struct(struct io_struct *io);
void Delete_chk_log_entry_struct(struct chk_log_entry_struct *log);
void Delete_log_entry_struct(struct log_entry_struct *log);
void Delete_log_struct(struct log_struct *log);

extern PREFIX void Print_msg(struct msg_struct *msg);
void Print_status_struct(struct status_struct *status);
void Print_control_struct(struct control_struct *ctrl);
void Print_param_struct(struct param_struct *param);
void Print_cmd_struct(struct cmd_struct *cmd);
void Print_supp_cmd_struct(struct supp_cmd_struct *supp_cmd);
void Print_io_def_struct(struct io_def_struct   *io_def);
void Print_io_struct(struct io_struct   *io);
void Print_log_struct(struct log_struct *log);
void Print_log_entry_struct(struct log_entry_struct *entry);
void Print_chk_log_entry_struct(struct chk_log_entry_struct *entry);

int  String_contains_xml_chars(char *string);

struct xtndString {
  int   lenFree;
  char *str;
};

/** Enumeration of the various possible states of our SAX parser
    for the results of an OGSI findServiceData
    - corresponds to the elements of the doc we're interested in */
enum doc_state {UNKNOWN, STARTING, 
		OGSI_ENTRY, MEMBER_SERVICE_LOCATOR, 
		GS_HANDLE, CONTENT, SERVICE_TYPE, COMPONENT_CONTENT, 
		COMPONENT_START_DATE_TIME,
		COMPONENT_CREATOR_NAME, COMPONENT_CREATOR_GROUP, 
		COMPONENT_SOFTWARE_PACKAGE, COMPONENT_TASK_DESCRIPTION, 
		/* The next line has those that are WSRF-specifc states */
		WSRF_ENTRY, MEMBER_SERVICE_EPR, SERVICEGROUP_ENTRY_EPR,
		EPR, WSADDRESS, SERVICEGROUP_EPR, SERVICEGROUP_WSADDRESS,
		FINISHING};

/** Holds the state for the SAX parser for the results of
    an OGSI findServiceData */
struct ParserState {

  int return_val;
  /** How many entries we currently have */
  int num_entries;
  /** How many entries we can store */
  int max_entries;
  /** Where we are in the document tree */
  enum doc_state depth;
  /** Pointer to array of structs holding entry details */
  struct registry_entry *entries;
};

/** Uses SAX to parse the document returned by doing a findServiceData
 *  on a serviceGroupRegistration service.
 */
int Parse_registry_entries(char* buf, int size, 
			   int *num_entries, 
			   struct registry_entry **entries);

/** SAX event handler for OGSI findServiceData results */
void Start_element_handler(void           *user_data,
			   const xmlChar  *name,
			   const xmlChar **attrs);

/** SAX event handler for OGSI findServiceData results */
void End_element_handler(void          *user_data,
			 const xmlChar *name);

/** SAX event handler for OGSI findServiceData results */
void Characters_handler(void          *user_data,
			const xmlChar *ch,
			int  	       len);

/** Parse the supplied resource property document and pull out the
    value of the specified resource property */
int Extract_resource_property(char *pRPDoc, 
			      int   size,
			      char *name,
			      char *resultBuf,
			      int   resultBufLen);

/** Check to see whether or not this message has been seen within the 
    last storeSize messages received 
    @internal */
int Msg_already_received(char *msg_uid,
			 struct msg_uid_history_struct *hist);

int Delete_msg_store(struct msg_store_struct *msgStore);
int Delete_msg_uid_store(struct msg_uid_history_struct *uidHist);

#endif
