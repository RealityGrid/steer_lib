/*----------------------------------------------------------------------------
  This header file contains defines routines and data structures for 
  parsing the XML steering-communication messages

  (C) Copyright 2002, 2004, University of Manchester, United Kingdom,
  all rights reserved.

  This software is produced by the Supercomputing, Visualization and
  e-Science Group, Manchester Computing, University of Manchester
  as part of the RealityGrid project (http://www.realitygrid.org),
  funded by the EPSRC under grants GR/R67699/01 and GR/R67699/02.

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

#ifndef __REG_STEER_XML_H__
#define __REG_STEER_XML_H__

#include "libxml/xmlmemory.h"
#include "libxml/parser.h"

#define REG_SUCCESS 0
#define REG_FAILURE 1

/*-----------------------------------------------------------------*/

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

struct cmd_struct{

  xmlChar             *id;
  xmlChar             *name;
  struct param_struct *first_param;
  struct param_struct *param;
  struct cmd_struct   *next;
};

struct status_struct{

  struct param_struct *first_param;
  struct param_struct *param;
  struct cmd_struct   *first_cmd;
  struct cmd_struct   *cmd;
};

struct io_struct{

  xmlChar  	   *label;
  xmlChar  	   *handle;
  xmlChar  	   *direction;
  xmlChar  	   *freq_handle;
  struct io_struct *next;
};

struct control_struct{

  struct param_struct *first_param;
  struct param_struct *param;
  struct cmd_struct   *first_cmd;
  struct cmd_struct   *cmd;
};

struct supp_cmd_struct{

  struct cmd_struct   *first_cmd;
  struct cmd_struct   *cmd;
};

struct io_def_struct{
  
  struct io_struct *first_io;
  struct io_struct *io;
};

struct chk_log_entry_struct{

  xmlChar                 *chk_handle;
  xmlChar                 *chk_tag;
  struct param_struct     *first_param;
  struct param_struct     *param;
  struct chk_log_entry_struct *next;
};

struct log_entry_struct{

  xmlChar                 *key;
  struct param_struct     *first_param_log;
  struct param_struct     *param_log;
  struct chk_log_entry_struct *first_chk_log;
  struct chk_log_entry_struct *chk_log;
  struct log_entry_struct *next;
};

struct log_struct{

  struct log_entry_struct *first_entry;
  struct log_entry_struct *entry;
};

struct msg_struct{

  int   	           msg_type;  
  struct status_struct    *status;
  struct control_struct   *control;
  struct supp_cmd_struct  *supp_cmd;
  struct io_def_struct    *io_def;
  struct io_def_struct    *chk_def;
  struct log_struct       *log;
};

/*-----------------------------------------------------------------*/

int Parse_xml(xmlDocPtr doc, struct msg_struct *msg);
int Parse_xml_file(char* filename, struct msg_struct *msg);
int Parse_xml_buf(char* buf, int size, struct msg_struct *msg);

int parseStatus(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	        struct status_struct *status);
int parseControl(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	         struct control_struct *ctrl);
int parseSuppCmd(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		 struct supp_cmd_struct *supp_cmd);
int parseParam(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	       struct param_struct *param);
int parseCmd(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	     struct cmd_struct *cmd);
int parseChkTypeDef(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		    struct io_def_struct *chk_def);
int parseIOTypeDef(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		   struct io_def_struct *io_def);
int parseIOType(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
                 struct io_struct *io);
int parseLog(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	     struct log_struct *log);
int parseLogEntry(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		  struct log_entry_struct *log_entry);
int parseChkLogEntry(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		     struct chk_log_entry_struct *log_entry);

struct msg_struct           *New_msg_struct();
struct status_struct        *New_status_struct();
struct control_struct       *New_control_struct();
struct supp_cmd_struct      *New_supp_cmd_struct();
struct io_def_struct        *New_io_def_struct();
struct io_struct            *New_io_struct();
struct param_struct         *New_param_struct();
struct cmd_struct           *New_cmd_struct();
struct chk_log_entry_struct *New_chk_log_entry_struct();
struct log_entry_struct     *New_log_entry_struct();
struct log_struct           *New_log_struct();

void Delete_msg_struct(struct msg_struct *msg);
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

void Print_msg(struct msg_struct *msg);
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

/* Enumeration of the various possible states of our parser
   - corresponds to the elements of the doc we're interested in */
enum doc_state {UNKNOWN, STARTING, OGSI_ENTRY, MEMBER_SERVICE_LOCATOR, 
		GS_HANDLE, CONTENT, SERVICE_TYPE, COMPONENT_CONTENT, 
		COMPONENT_START_DATE_TIME,
		COMPONENT_CREATOR_NAME, COMPONENT_CREATOR_GROUP, 
		COMPONENT_SOFTWARE_PACKAGE, COMPONENT_TASK_DESCRIPTION, 
		FINISHING};

struct ParserState {

  int return_val;
  /* How many entries we currently have */
  int num_entries;
  /* How many entries we can store */
  int max_entries;
  /* Where we are in the document tree */
  enum doc_state depth;
  /* Pointer to array of structs holding entry details */
  struct registry_entry *entries;
};

/* Uses SAX to parse the document returned by doing a findServiceData
 * on a serviceGroupRegistration service.
 */
int Parse_registry_entries(char* buf, int size, 
			   int *num_entries, 
			   struct registry_entry **entries);

/* SAX event handler */
void Start_element_handler(void           *user_data,
			   const xmlChar  *name,
			   const xmlChar **attrs);

/* SAX event handler */
void End_element_handler(void          *user_data,
			 const xmlChar *name);

/* SAX event handler */
void Characters_handler(void          *user_data,
			const xmlChar *ch,
			int  	       len);

#endif
