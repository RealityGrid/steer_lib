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

  Author: Andrew Porter
          Robert Haines
 */

/** @internal
    @file ReG_Steer_XML.c
    @author Andrew Porter
    @author Robert Haines
    @brief Routines and structures for parsing xml documents */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "ReG_Steer_Browser.h"
#include "ReG_Steer_Steerside.h"

/** Declared in ReG_Steer_Appside.c */
extern struct msg_store_struct  Msg_store;
extern struct msg_store_struct *Msg_store_tail;

/** Declared in ReG_Steer_Appside.c */
extern struct msg_uid_history_struct Msg_uid_store;

#if REG_VALIDATE_XML
/* The schema to validate against, compiled into the library */
extern unsigned int reg_steer_comm_xsd_len;
extern unsigned char reg_steer_comm_xsd[];
#endif

/*-----------------------------------------------------------------*/

int Parse_xml_file(char* filename, struct msg_struct *msg,
		   Sim_entry_type *sim)
{
  xmlDocPtr doc;

  doc = xmlParseFile(filename);

  if (doc == NULL){
    fprintf(stderr, "STEER: Parse_xml_file: Hit error parsing file %s\n",
	    filename);
    return REG_FAILURE;
  }

  return Parse_xml(doc, msg, sim);
}

/*-----------------------------------------------------------------*/

int Parse_xml_buf(char* buf, int size, struct msg_struct *msg,
		  Sim_entry_type *sim)
{
  xmlDocPtr doc;

  if(!buf){

    fprintf(stderr, "STEER: Parse_xml_buf: ptr to buffer is NULL\n");
    return REG_FAILURE;
  }

#if defined(REG_DEBUG_FULL) || !defined(REG_HAVE_XMLREADMEMORY)
  doc = xmlParseMemory(buf, size);
#else
  /* Use alternate call so can turn off error messages from parser */
  doc = xmlReadMemory(buf, size, "http://www.realitygrid.org", NULL,
		      XML_PARSE_NOWARNING | XML_PARSE_NOERROR );
#endif

  if (doc == NULL){
    fprintf(stderr, "STEER: Parse_xml_buf: Hit error parsing buffer\n");
    return REG_FAILURE;
  }

  return Parse_xml(doc, msg, sim);
}

/*-----------------------------------------------------------------*/

int Parse_xml(xmlDocPtr doc, struct msg_struct *msg,
	      Sim_entry_type *sim)
{
#if REG_VALIDATE_XML
  xmlNsPtr   ns;
#endif
  xmlNodePtr cur;
#ifdef REG_DEBUG_FULL
  struct msg_store_struct *cur_msg;
#endif

  cur = xmlDocGetRootElement(doc);
  if (cur == NULL) {
      fprintf(stderr,"STEER: Parse_xml: empty document\n");
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
  }

#if REG_VALIDATE_XML
  if(Validate_xml(doc) != REG_SUCCESS) {
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
  }

  ns = xmlSearchNsByHref(doc, cur, (const xmlChar *) REG_STEER_NAMESPACE);
  if(ns == NULL) {
    fprintf(stderr, "STEER: Parse_xml: document of the wrong type.\n       "
	    "Namespace '%s' not found\n", REG_STEER_NAMESPACE);
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }
#endif

  if (!xmlStrcmp(cur->name, (const xmlChar *) "ReG_steer_message")) {
#ifdef REG_DEBUG_FULL
    fprintf(stderr,"STEER: Parse_xml: Have ReG_steer_message doc\n");
#endif

    if(parseSteerMessage(doc, cur, msg, sim) != REG_SUCCESS) {
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
    }

    /* Print out what we've got */
#ifdef REG_DEBUG_FULL
    if(msg && msg->msg_type != MSG_NOTSET){
      fprintf(stderr, "STEER: Parse_xml: Calling Print_msg...\n");
      Print_msg(msg);
    }
#endif
  }
  else if (!xmlStrcmp(cur->name, (const xmlChar *) "ResourceProperties") ||
	   !xmlStrcmp(cur->name, (const xmlChar *) "controlMsg")) {
#ifdef REG_DEBUG_FULL
    fprintf(stderr,"STEER: Parse_xml: passing %s to "
	    "parseResourceProperties...\n", (char *)(cur->name));
#endif
    parseResourceProperties(doc, cur, sim);

    /* Print out what we've got */
#ifdef REG_DEBUG_FULL
    if(sim){
      cur_msg = &(sim->Msg_store);
      while(cur_msg){
	fprintf(stderr, "STEER: Parse_xml: Calling Print_msg...\n");
	if(cur_msg->msg)Print_msg(cur_msg->msg);
	cur_msg = cur_msg->next;
      }
    }
#endif
  }
  else{
    fprintf(stderr,"STEER: Parse_xml: document of the wrong type, root node "
	    "= >%s< != ReG_steer_message or ResourceProperties or "
	    "controlMsg\n", (char *)(cur->name));
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }

  /* Clean up everything else before quitting. */
  xmlFreeDoc(doc);
  xmlCleanupParser();

  return REG_SUCCESS;
}

#if REG_VALIDATE_XML
int Validate_xml(xmlDocPtr doc) {
  xmlSchemaParserCtxtPtr schema_ctxt;
  xmlSchemaValidCtxtPtr schema_valid;
  xmlSchemaPtr schema;
  int result = REG_SUCCESS;

  /* Have to reload the schema each time. Can't simply load it
     once then reuse it for some reason. */
  schema_ctxt = xmlSchemaNewMemParserCtxt(reg_steer_comm_xsd,
					  reg_steer_comm_xsd_len);
  schema = xmlSchemaParse(schema_ctxt);
  schema_valid = xmlSchemaNewValidCtxt(schema);

  if(xmlSchemaValidateDoc(schema_valid, doc) != 0) {
    fprintf(stderr, "STEER: Validate_xml: message could not be validated "
	    "against its schema.");
    result = REG_FAILURE;
  }

  xmlSchemaFreeValidCtxt(schema_valid);
  xmlSchemaFree(schema);
  xmlSchemaFreeParserCtxt(schema_ctxt);

  return result;
}
#endif

/*-----------------------------------------------------------------*/
/** Parse the supplied resource property document and pull out the
    value of the specified resource property */
int Extract_resource_property(char *pRPDoc,
			      int   size,
			      char *name,
			      char *resultBuf,
			      int   resultBufLen)
{
  char *pStart, *pStop;
  int   len;

  if(!pRPDoc){
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Extract_resource_property: ptr to RP "
	    "document is NULL\n");
#endif
    return REG_FAILURE;
  }

  if( !(pStart = strstr(pRPDoc, name)) ){
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Extract_resource_property: RP %s not found\n",
	    name);
#endif
    return REG_FAILURE;
  }
  /* Move ptr forwards to point to beginning of content - allow for closing
     angle bracket */
  pStart = strchr(pStart, '>');
  pStart++;

  if(!(pStop = strstr(pStart, name))){
#ifdef REG_DEBUG
   fprintf(stderr, "STEER: Extract_resource_property: closing tag for "
	    "RP %s not found\n", name);
#endif
    return REG_FAILURE;
  }

  /* Copy the contents into our result buffer - -2 is to allow for '</' of
     closing element tag */
  len = (int)(pStop - 2 - pStart);
  strncpy(resultBuf, pStart, len);
  resultBuf[len] = '\0';

  /*
#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Extract_resource_property: Value of RP "
	  "%s = >>%s<<\n", name, resultBuf);
#endif
  */
  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseResourceProperties(xmlDocPtr doc, xmlNodePtr cur,
			    Sim_entry_type *sim) {
  xmlNodePtr child;
  xmlChar   *steerStatus;
  struct msg_store_struct *curMsg;

  /* If we've been called by a steering client then must store
     any messages in a structure associated with the simulation being
     steered as may be one of many */
  if(sim){
    curMsg = sim->Msg_store_tail;
  }
  else{
    /* Otherwise, we store the messages we receive in a global structure
       from which they are extracted in order */
    curMsg = Msg_store_tail;
  }

  /* Check where we are in tree; we might have been given the root node
     of a ResourceProperties document or we might have been given the
     root node of a ResourceProperty directly */
  if( !xmlStrcmp(cur->name, (const xmlChar *) "ResourceProperties") ){
    cur = cur->xmlChildrenNode;
  }

  /* Walk the tree - search for first non-blank node */
  while ( cur ){
    if(xmlIsBlankNode ( cur ) ){
      cur = cur -> next;
      continue;
    }

    if( !xmlStrcmp(cur->name, (const xmlChar *) "supportedCommands") ||
	!xmlStrcmp(cur->name, (const xmlChar *) "paramDefinitions") ||
	!xmlStrcmp(cur->name, (const xmlChar *) "ioTypeDefinitions") ||
	!xmlStrcmp(cur->name, (const xmlChar *) "chkTypeDefinitions") ||
	!xmlStrcmp(cur->name, (const xmlChar *) "controlMsg") ||
	!xmlStrcmp(cur->name, (const xmlChar *) "statusMsg") ){
      child = cur->xmlChildrenNode;
      while ( child && xmlIsBlankNode ( child ) ){ child = child -> next; }

      if (child && !xmlStrcmp(child->name,
			      (const xmlChar *) "ReG_steer_message")) {
#ifdef REG_DEBUG_FULL
	fprintf(stderr, "STEER: parseResourceProperties: Calling "
		"parseSteerMessage...\n");
#endif
	curMsg->msg = New_msg_struct();

	if(parseSteerMessage(doc, child, curMsg->msg, sim) !=
	   REG_SUCCESS){
	  Delete_msg_struct(&(curMsg->msg));
	  curMsg->next = NULL;
	}
	else{
#ifdef REG_DEBUG
	  fprintf(stderr, "STEER: parseResourceProperties: storing msg "
		  "with UID %s\n", curMsg->msg->msg_uid);
#endif
	  curMsg->next = New_msg_store_struct();
	  curMsg = curMsg->next;
	}
      }
      else{
	fprintf(stderr, "STEER: parseResourceProperties: ERROR: have not"
		" got a ReG_steer_message\n");
	return REG_FAILURE;
      }
    }
    else if(!sim && !xmlStrcmp(cur->name, (const xmlChar *) "steererStatus")){

      steerStatus = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(!xmlStrcmp(steerStatus, (const xmlChar *)"DETACHED")){

	/* Pretend we've received a detach command - aids compatibility
	   with higher levels of library */
	curMsg->msg = New_msg_struct();
	curMsg->msg->control = New_control_struct();
	curMsg->msg->control->first_cmd = New_cmd_struct();
	curMsg->msg->control->cmd = curMsg->msg->control->first_cmd;
	curMsg->msg->control->cmd->name = (xmlChar *)xmlMalloc(16);
	sprintf((char *)curMsg->msg->control->first_cmd->name, "DETACH");

	curMsg->next = New_msg_store_struct();
	curMsg = curMsg->next;
      }
    }
#ifdef REG_DEBUG_FULL
    else{
      fprintf(stderr, "STEER: parseResourceProperties: ignoring node: %s\n",
	      (char *)(cur->name));
    }
#endif

    cur = cur -> next;
  }

  if(sim){
    sim->Msg_store_tail = curMsg;
  }
  else{
    Msg_store_tail = curMsg;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseSteerMessage(xmlDocPtr doc, xmlNodePtr cur,
		      struct msg_struct *msg, Sim_entry_type *sim) {
  struct msg_uid_history_struct *uidStorePtr;

  /* If we've been called by a steering client then must store
     any messages in a structure associated with the simulation being
     steered as may be one of many */
  if(sim){
    uidStorePtr = &(sim->Msg_uid_store);
  }
  else{
    /* Otherwise, we store the messages we receive in a global structure
       from which they are extracted in order */
    uidStorePtr = &Msg_uid_store;
  }

  /* Get the msg UID if present */
  if((msg->msg_uid = xmlGetProp(cur, (const xmlChar*) "UID"))) {
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: msg UID = %s\n",
	    (char*)(msg->msg_uid));
#endif /* REG_DEBUG_FULL */
    /* Check that we haven't already seen this message
       before we bother to store it */
    if( Msg_already_received((char*)(msg->msg_uid), uidStorePtr) ){
#ifdef REG_DEBUG_FULL
      fprintf(stderr, "STEER: INFO: parseSteerMessage: msg"
	      " with UID %s has been seen before\n",
	      msg->msg_uid);
#endif /* REG_DEBUG_FULL */
      /* We have - skip this one */
      msg->msg_type = MSG_NOTSET;
      return REG_FAILURE;
    }
#ifdef REG_DEBUG_FULL
    else{
      fprintf(stderr, "STEER: INFO: parseSteerMessage: msg"
	      " with UID %s has NOT been seen before\n",
	      msg->msg_uid);
    }
#endif /* REG_DEBUG_FULL */
  }

  /* Walk the tree */
  cur = cur->xmlChildrenNode;
  while ( cur && xmlIsBlankNode ( cur ) ){

        cur = cur -> next;
  }
  if ( cur == 0 ){
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }

  /* Get the type of the message and record it */
  msg->msg_type = Get_message_type((const char *)(cur->name));

  switch(msg->msg_type){

  case STATUS:
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Calling "
	    "parseStatus...\n");
#endif
    msg->status = New_status_struct();
    parseStatus(doc, cur, msg->status);
    break;

  case CONTROL:
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Calling "
	    "parseControl...\n");
#endif
    msg->control = New_control_struct();
    parseControl(doc, cur, msg->control);
   break;

  case SUPP_CMDS:
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Calling "
	    "parseSuppCmd...\n");
#endif
    msg->supp_cmd = New_supp_cmd_struct();
    parseSuppCmd(doc, cur, msg->supp_cmd);
    break;

  case PARAM_DEFS:
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Calling "
	    "parseStatus...\n");
#endif
    /* Use code for 'status' messages because one
       encapsulates the other */
    msg->status = New_status_struct();
    parseStatus(doc, cur, msg->status);
    break;

  case IO_DEFS:
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Calling "
	    "parseIOTypeDef...\n");
#endif
    msg->io_def = New_io_def_struct();
    parseIOTypeDef(doc, cur, msg->io_def);
    break;

  case CHK_DEFS:
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Calling "
	    "parseChkTypeDef...\n");
#endif
    msg->chk_def = New_io_def_struct();
    parseChkTypeDef(doc, cur, msg->chk_def);
    break;

  case STEER_LOG:
#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Calling parseLog...\n");
#endif
    msg->log = New_log_struct();
    parseLog(doc, cur, msg->log);
    break;

  default:
    fprintf(stderr, "STEER: INFO: parseSteerMessage: Unrecognised message"
	    " type %d for message name >>%s<<\n",
	    msg->msg_type, (const char *)(cur->name));
    break;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseStatus(xmlDocPtr doc, xmlNodePtr cur, struct status_struct *status) {

  if(status == NULL){
    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Param") ){

      if( !status->first_param ){

	status->first_param = New_param_struct();
	status->param = status->first_param;
      }
      else{
        status->param->next = New_param_struct();
        status->param = status->param->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Calling parseParam...\n");
#endif
      parseParam(doc, cur, status->param);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Command") ){

      if( !status->first_cmd ){

	status->first_cmd = New_cmd_struct();
	status->cmd = status->first_cmd;
      }
      else{
	status->cmd->next = New_cmd_struct();
	status->cmd = status->cmd->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Calling parseCmd...\n");
#endif
      parseCmd(doc, cur, status->cmd);
    }

    cur = cur->next;
  }
  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseControl(xmlDocPtr doc, xmlNodePtr cur, struct control_struct *ctrl) {
  if(!ctrl){
    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;

  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Valid_after") ){

      ctrl->valid_after = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Param") ){

      if( !ctrl->first_param ){

	ctrl->first_param = New_param_struct();
	ctrl->param = ctrl->first_param;
      }
      else{
        ctrl->param->next = New_param_struct();
        ctrl->param = ctrl->param->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Calling parseParam...\n");
#endif
      parseParam(doc, cur, ctrl->param);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Command") ){

      if( !ctrl->first_cmd ){

	ctrl->first_cmd = New_cmd_struct();
	ctrl->cmd = ctrl->first_cmd;
      }
      else{
	ctrl->cmd->next = New_cmd_struct();
	ctrl->cmd = ctrl->cmd->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Calling parseCmd...\n");
#endif
      parseCmd(doc, cur, ctrl->cmd);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseIOTypeDef(xmlDocPtr doc, xmlNodePtr cur,
		   struct io_def_struct *io_def) {

  if(!io_def){

    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;

  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *) "IOType") ){

      if(!io_def->first_io){

	io_def->first_io = New_io_struct();
	io_def->io = io_def->first_io;
      }
      else{
	io_def->io->next = New_io_struct();
	io_def->io = io_def->io->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: parseIOTypeDef: Calling parseIOType...\n");
#endif
      parseIOType(doc, cur, io_def->io);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseChkTypeDef(xmlDocPtr doc, xmlNodePtr cur,
		    struct io_def_struct *chk_def) {

  if(!chk_def){

    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;

  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *) "ChkType") ){

      if(!chk_def->first_io){

	chk_def->first_io = New_io_struct();
	chk_def->io = chk_def->first_io;
      }
      else{
	chk_def->io->next = New_io_struct();
	chk_def->io = chk_def->io->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: parseChkTypeDef: Calling parseIOType...\n");
#endif
      parseIOType(doc, cur, chk_def->io);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseIOType(xmlDocPtr doc, xmlNodePtr cur, struct io_struct *io) {
  if(!io){

    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Handle") ) {

      io->handle = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Label") ){

      io->label = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Direction") ){

      io->direction = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Freq_handle") ){

      io->freq_handle = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseSuppCmd(xmlDocPtr doc, xmlNodePtr cur,
		 struct supp_cmd_struct *supp_cmd) {
  if(supp_cmd == NULL){
    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;
  while(cur != NULL){

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Command") ){

      if( !supp_cmd->first_cmd ){

	supp_cmd->first_cmd = New_cmd_struct();
	supp_cmd->cmd = supp_cmd->first_cmd;
      }
      else{
	supp_cmd->cmd->next = New_cmd_struct();
	supp_cmd->cmd = supp_cmd->cmd->next;
      }

#ifdef REG_DEBUG_FULL
      fprintf(stderr, "STEER: parseSuppCmd: Calling parseCmd...\n");
#endif
      parseCmd(doc, cur, supp_cmd->cmd);
    }
#ifdef REG_DEBUG
    else{
      fprintf(stderr, "STEER: parseSuppCmd: name = %s <> Command\n", cur->name);
    }
#endif

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseLog(xmlDocPtr doc, xmlNodePtr cur, struct log_struct *log) {
  if (!log) return REG_FAILURE;

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *)"Log_entry") ){

      if(!log->first_entry){

	log->first_entry = New_log_entry_struct();
	log->entry = log->first_entry;
      }
      else{
	log->entry->next = New_log_entry_struct();
	log->entry = log->entry->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: parseLog: calling parseLogEntry\n");
#endif
      parseLogEntry(doc, cur, log->entry);

    }
    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseLogEntry(xmlDocPtr doc, xmlNodePtr cur, struct log_entry_struct *log) {
  int return_status = REG_SUCCESS;

  if(!log) return REG_FAILURE;

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *)"Key") ){

      log->key = xmlNodeListGetString(doc,
				      cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *)"Chk_log_entry") ){
      if(!log->first_chk_log){

	log->first_chk_log = New_chk_log_entry_struct();
	log->chk_log = log->first_chk_log;
      }
      else{
	log->chk_log->next = New_chk_log_entry_struct();
	log->chk_log = log->chk_log->next;
      }
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: parseLogEntry: calling parseChkLogEntry\n");
#endif
      return_status = parseChkLogEntry(doc, cur, log->chk_log);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *)"Param") ){
      if(!log->first_param_log){

	log->first_param_log = New_param_struct();
	log->param_log = log->first_param_log;
      }
      else{
	log->param_log->next = New_param_struct();
	log->param_log = log->param_log->next;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: parseLogEntry: calling parseParam\n");
#endif
      return_status = parseParam(doc, cur, log->param_log);
    }

    cur = cur->next;
  }
  return return_status;
}

/*-----------------------------------------------------------------*/

int parseChkLogEntry(xmlDocPtr doc, xmlNodePtr cur,
		     struct chk_log_entry_struct *log_entry) {
  int return_status = REG_SUCCESS;

  if(!log_entry) return REG_FAILURE;

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if(!xmlStrcmp(cur->name, (const xmlChar *)"Chk_handle") ){

      log_entry->chk_handle = xmlNodeListGetString(doc,
					    cur->xmlChildrenNode, 1);
    }
    else if(!xmlStrcmp(cur->name, (const xmlChar *)"Chk_tag")){

      log_entry->chk_tag = xmlNodeListGetString(doc,
					      cur->xmlChildrenNode, 1);
    }
    else if(!xmlStrcmp(cur->name, (const xmlChar *)"Param")){

      if(!log_entry->first_param){

	log_entry->first_param = New_param_struct();
	log_entry->param = log_entry->first_param;
      }
      else{
	log_entry->param->next = New_param_struct();
	log_entry->param = log_entry->param->next;
      }

      return_status = parseParam(doc, cur, log_entry->param);
    }

    cur = cur->next;
  }

  return return_status;
}

/*-----------------------------------------------------------------*/

int parseParam(xmlDocPtr doc, xmlNodePtr cur, struct param_struct *param) {
  if(param == NULL){

    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *)"Handle") ){

      param->handle = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Label") ){

      param->label = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Value") ){

      param->value = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Steerable") ){

      param->steerable = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Type") ){

      param->type = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Is_internal") ){

      param->is_internal = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Min_value") ){

      param->min_val = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Max_value") ){

      param->max_val = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseCmd(xmlDocPtr doc, xmlNodePtr cur, struct cmd_struct *cmd) {
  if(!cmd){
    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;

  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Cmd_id") ){

      cmd->id = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Cmd_name") ){

      cmd->name = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Cmd_param") ){

      if(cmd->first_param){

	cmd->param->next = New_param_struct();
        cmd->param = cmd->param->next;
      }
      else{
	cmd->first_param = New_param_struct();
	cmd->param = cmd->first_param;
      }

      parseParam(doc, cur, cmd->param);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;

}

/*-----------------------------------------------------------------*/

struct msg_store_struct *New_msg_store_struct()
{
  struct msg_store_struct *store;

  store = (struct msg_store_struct *)malloc(sizeof(struct msg_store_struct));

  if(store){
    store->msg = NULL;
    store->next = NULL;
  }

  return store;
}

/*-----------------------------------------------------------------*/

struct msg_struct *New_msg_struct()
{

  struct msg_struct *msg;

  msg = (struct msg_struct *)malloc(sizeof(struct msg_struct));

  if(msg){

    msg->msg_type = MSG_NOTSET;
    msg->msg_uid  = NULL;
    msg->status   = NULL;
    msg->control  = NULL;
    msg->supp_cmd = NULL;
    msg->io_def   = NULL;
    msg->chk_def  = NULL;
    msg->log      = NULL;
  }

  return msg;
}

/*-----------------------------------------------------------------*/

struct status_struct *New_status_struct()
{
  struct status_struct *status;

  status = (struct status_struct *)malloc(sizeof(struct status_struct));

  if(status){

    status->first_param = NULL;
    status->param       = NULL;

    status->first_cmd = NULL;
    status->cmd       = NULL;
  }

  return status;
}

/*-----------------------------------------------------------------*/

struct control_struct *New_control_struct()
{
  struct control_struct *ctrl;

  ctrl = (struct control_struct *)malloc(sizeof(struct control_struct));

  if(ctrl){
    ctrl->valid_after = NULL;
    ctrl->first_param = NULL;
    ctrl->param       = NULL;
    ctrl->first_cmd   = NULL;
    ctrl->cmd         = NULL;
  }

  return ctrl;
}
/*-----------------------------------------------------------------*/

struct io_def_struct *New_io_def_struct()
{
  struct io_def_struct *io_def;

  io_def = (struct io_def_struct *)malloc(sizeof(struct io_def_struct));

  if(io_def){

    io_def->first_io = NULL;
    io_def->io       = NULL;
  }

  return io_def;
}

/*-----------------------------------------------------------------*/

struct io_struct *New_io_struct()
{
  struct io_struct *io;

  io = (struct io_struct *)malloc(sizeof(struct io_struct));

  if(io){

    io->label        = NULL;
    io->handle       = NULL;
    io->direction    = NULL;
    io->freq_handle  = NULL;
    io->next         = NULL;
  }

  return io;
}

/*-----------------------------------------------------------------*/

struct param_struct *New_param_struct()
{
  struct param_struct *param;

  param = (struct param_struct *)malloc(sizeof(struct param_struct));

  if(param){
    param->handle = NULL;
    param->label  = NULL;
    param->value  = NULL;
    param->steerable   = NULL;
    param->type        = NULL;
    param->is_internal = NULL;
    param->min_val     = NULL;
    param->max_val     = NULL;
    param->next        = NULL;
  }

  return param;
}

/*-----------------------------------------------------------------*/

struct supp_cmd_struct *New_supp_cmd_struct()
{
  struct supp_cmd_struct *supp_cmd;

  supp_cmd = (struct supp_cmd_struct *)malloc(sizeof(struct supp_cmd_struct));

  if(supp_cmd){

    supp_cmd->cmd       = NULL;
    supp_cmd->first_cmd = NULL;
  }

  return supp_cmd;
}

/*-----------------------------------------------------------------*/

struct cmd_struct *New_cmd_struct()
{
  struct cmd_struct *cmd;

  cmd = (struct cmd_struct *)malloc(sizeof(struct cmd_struct));

  if(cmd){

    cmd->id          = NULL;
    cmd->name        = NULL;
    cmd->first_param = NULL;
    cmd->param       = NULL;
    cmd->next        = NULL;
  }

  return cmd;
}

/*-----------------------------------------------------------------*/

struct log_struct *New_log_struct()
{
  struct log_struct *log;

  log = (struct log_struct *)malloc(sizeof(struct log_struct));

  if(log){

    log->first_entry = NULL;
    log->entry       = NULL;
  }

  return log;
}

/*-----------------------------------------------------------------*/

struct log_entry_struct *New_log_entry_struct()
{
  struct log_entry_struct *entry;

  entry = (struct log_entry_struct *)
                      malloc(sizeof(struct log_entry_struct));

  if(entry){

    entry->key             = NULL;
    entry->first_chk_log   = NULL;
    entry->chk_log         = NULL;
    entry->first_param_log = NULL;
    entry->param_log       = NULL;
    /* Steer log stuct not yet implemented */
    /*entry->first_steer_log = NULL;*/
    entry->next            = NULL;
  }

  return entry;
}

/*-----------------------------------------------------------------*/

struct chk_log_entry_struct *New_chk_log_entry_struct()
{
  struct chk_log_entry_struct *entry;

  entry = (struct chk_log_entry_struct *)
                      malloc(sizeof(struct chk_log_entry_struct));

  if(entry){

    entry->chk_handle  = NULL;
    entry->chk_tag     = NULL;
    entry->param       = NULL;
    entry->first_param = NULL;
    entry->next        = NULL;
  }

  return entry;
}

/*-----------------------------------------------------------------*/

void Delete_msg_struct(struct msg_struct **msgIn)
{
  struct msg_struct *msg = *msgIn;
  if (!msg) return;

  if(msg->msg_uid){
    xmlFree(msg->msg_uid);
    msg->msg_uid = NULL;
  }

  if(msg->status){

    Delete_status_struct(msg->status);
    msg->status = NULL;
  }

  if(msg->control){

    Delete_control_struct(msg->control);
    msg->control = NULL;
  }

  if(msg->supp_cmd){

    Delete_supp_cmd_struct(msg->supp_cmd);
    msg->supp_cmd = NULL;
  }

  if(msg->io_def){

    Delete_io_def_struct(msg->io_def);
    msg->io_def = NULL;
  }

  if(msg->chk_def){

    Delete_io_def_struct(msg->chk_def);
    msg->chk_def = NULL;
  }

  if(msg->log){

    Delete_log_struct(msg->log);
    msg->log = NULL;
  }

  free(*msgIn);
  *msgIn = NULL;
}

/*-----------------------------------------------------------------*/

void Delete_status_struct(struct status_struct *status)
{
  if (!status) return;

  if(status->first_param){

    Delete_param_struct(status->first_param);
    status->first_param = NULL;
    status->param       = NULL;
  }

  if(status->first_cmd){

    Delete_cmd_struct(status->first_cmd);
    status->first_cmd = NULL;
    status->cmd       = NULL;
  }

  free(status);
}

/*-----------------------------------------------------------------*/

void Delete_control_struct(struct control_struct *ctrl)
{
  if (!ctrl) return;

  if (ctrl->valid_after) xmlFree(ctrl->valid_after);

  if(ctrl->first_param){

    Delete_param_struct(ctrl->first_param);
    ctrl->first_param = NULL;
    ctrl->param       = NULL;
  }

  if(ctrl->first_cmd){

    Delete_cmd_struct(ctrl->first_cmd);
    ctrl->first_cmd = NULL;
    ctrl->cmd       = NULL;
  }

  free(ctrl);
}

/*-----------------------------------------------------------------*/

void Delete_supp_cmd_struct(struct supp_cmd_struct *supp_cmd)
{
  if (!supp_cmd) return;

  if(supp_cmd->first_cmd){

    Delete_cmd_struct(supp_cmd->first_cmd);
    supp_cmd->first_cmd = NULL;
    supp_cmd->cmd       = NULL;
  }

  free(supp_cmd);
}

/*-----------------------------------------------------------------*/

void Delete_cmd_struct(struct cmd_struct *cmd)
{
  struct cmd_struct *dum_ptr;

  while(cmd){

    if(cmd->id) xmlFree(cmd->id);
    if(cmd->name) xmlFree(cmd->name);

    if(cmd->first_param){
      Delete_param_struct(cmd->first_param);
      cmd->first_param = NULL;
      cmd->param       = NULL;
    }

    dum_ptr = cmd->next;
    free(cmd);
    cmd = dum_ptr;
  }
}

/*-----------------------------------------------------------------*/

void Delete_io_def_struct(struct io_def_struct *io_def)
{
  if(io_def){

    Delete_io_struct(io_def->first_io);
    free (io_def);
  }
}

/*-----------------------------------------------------------------*/

void Delete_io_struct(struct io_struct *io)
{
  struct io_struct *dum_ptr;

  while(io){

    if(io->label)       xmlFree(io->label);
    if(io->handle)      xmlFree(io->handle);
    if(io->direction)   xmlFree(io->direction);
    if(io->freq_handle) xmlFree(io->freq_handle);

    dum_ptr = io->next;
    free(io);
    io = dum_ptr;
  }
}

/*-----------------------------------------------------------------*/

void Delete_param_struct(struct param_struct *param)
{
  struct param_struct *dum_ptr;

  while(param){

    if (param->handle)      xmlFree(param->handle);
    if (param->label)       xmlFree(param->label);
    if (param->value)       xmlFree(param->value);
    if (param->type)        xmlFree(param->type);
    if (param->steerable)   xmlFree(param->steerable);
    if (param->is_internal) xmlFree(param->is_internal);
    if (param->min_val)     xmlFree(param->min_val);
    if (param->max_val)     xmlFree(param->max_val);

    dum_ptr = param->next;
    free(param);
    param = dum_ptr;
  }
}

/*-----------------------------------------------------------------*/

void Delete_chk_log_entry_struct(struct chk_log_entry_struct *log)
{
  struct chk_log_entry_struct *dum_ptr;

  while(log){

    xmlFree(log->chk_handle);
    xmlFree(log->chk_tag);

    Delete_param_struct(log->first_param);

    dum_ptr = log->next;
    free(log);
    log = dum_ptr;
  }
}

/*-----------------------------------------------------------------*/

void Delete_log_entry_struct(struct log_entry_struct *log)
{
  struct log_entry_struct *dum_ptr;

  while(log){

    xmlFree(log->key);

    Delete_chk_log_entry_struct(log->first_chk_log);
    Delete_param_struct(log->first_param_log);

    dum_ptr = log->next;
    free(log);
    log = dum_ptr;
  }
}

/*-----------------------------------------------------------------*/

void Delete_log_struct(struct log_struct *log)
{
  if(log){

    Delete_log_entry_struct(log->first_entry);
    free(log);
  }
}

/*-----------------------------------------------------------------*/

void Print_msg(struct msg_struct *msg)
{
  if (!msg) {

    fprintf(stderr, "STEER: Print_msg: ptr to msg struct is null\n");
    return;
  }

  if(msg->msg_type == MSG_ERROR){
    fprintf(stderr, "STEER: Print_msg: msg is ERROR message\n");
  }
  else if(msg->msg_type == MSG_NOTSET){
    fprintf(stderr, "STEER: Print_msg: msg type is unset\n");
  }
  else if(msg->msg_type == SUPP_CMDS){
    fprintf(stderr, "STEER: Print_msg: msg type is SUPP_CMDS\n");
  }
  else if(msg->msg_type == IO_DEFS){
    fprintf(stderr, "STEER: Print_msg: msg type is IO_DEFS\n");
  }
  else if(msg->msg_type == PARAM_DEFS){
    fprintf(stderr, "STEER: Print_msg: msg type is PARAM_DEFS\n");
  }
  else if(msg->msg_type == STATUS){
    fprintf(stderr, "STEER: Print_msg: msg type is STATUS\n");
  }
  else if(msg->msg_type == CONTROL){
    fprintf(stderr, "STEER: Print_msg: msg type is CONTROL\n");
  }
  else if(msg->msg_type == CHK_DEFS){
    fprintf(stderr, "STEER: Print_msg: msg type is CHK_DEFS\n");
  }
  else if(msg->msg_type == STEER_LOG){
    fprintf(stderr, "STEER: Print_msg: msg type is STEER_LOG\n");
  }

  if(msg->status){

    Print_status_struct(msg->status);
  }

  if(msg->control){

    Print_control_struct(msg->control);
  }

  if(msg->supp_cmd){

    Print_supp_cmd_struct(msg->supp_cmd);
  }

  if(msg->io_def){

    Print_io_def_struct(msg->io_def);
  }

  if(msg->chk_def){

    Print_io_def_struct(msg->chk_def);
  }

  if(msg->log){

    Print_log_struct(msg->log);
  }

  fprintf(stderr, "STEER: Print_msg: done\n");
}

/*-----------------------------------------------------------------*/

void Print_status_struct(struct status_struct *status)
{
  if (!status) return;

  if(status->first_param){

    Print_param_struct(status->first_param);
  }

  if(status->first_cmd){

    Print_cmd_struct(status->first_cmd);
  }

}

/*-----------------------------------------------------------------*/

void Print_control_struct(struct control_struct *ctrl)
{
  if (!ctrl) return;

  if(ctrl->first_param){

    Print_param_struct(ctrl->first_param);
  }

  if(ctrl->first_cmd){

    Print_cmd_struct(ctrl->first_cmd);
  }

}

/*-----------------------------------------------------------------*/

void Print_param_struct(struct param_struct *param)
{
  struct param_struct *ptr = param;
  int    count;

  count = 0;

  while(ptr){
    fprintf(stderr, "STEER: Param no. %d:\n", count);
    if(ptr->handle)     fprintf(stderr, "   Handle = %s\n",
				(char *)(ptr->handle));
    if(ptr->label)      fprintf(stderr, "   Label  = %s\n",
				(char *)(ptr->label));
    if(ptr->value)      fprintf(stderr, "   Value  = %s\n",
				(char *)(ptr->value));
    if(ptr->steerable)  fprintf(stderr, "   Steerable = %s\n",
			      (char *)(ptr->steerable));
    if(ptr->type)       fprintf(stderr, "   Type   = %s\n",
			    (char *)(ptr->type));
    if(ptr->is_internal)fprintf(stderr, "   Internal = %s\n",
				(char *)(ptr->is_internal));

    count++;
    ptr = ptr->next;
  }
}

/*-----------------------------------------------------------------*/

void Print_cmd_struct(struct cmd_struct *cmd)
{
  struct cmd_struct *ptr = cmd;
  int    count;

  count = 0;

  while(ptr){
    fprintf(stderr, "STEER: Command no. %d:\n", count);
    if(ptr->id)   fprintf(stderr, "   ID = %s\n", (char *)(ptr->id));
    if(ptr->name) fprintf(stderr, " name = %s\n", (char *)(ptr->name));
    if(ptr->first_param) Print_param_struct(cmd->first_param);

    count++;
    ptr = ptr->next;
  }
}
/*-----------------------------------------------------------------*/

void Print_supp_cmd_struct(struct supp_cmd_struct *supp_cmd)
{
  if (!supp_cmd) return;

  if(supp_cmd->first_cmd){

    fprintf(stderr, "STEER: Supported commands:\n");
    Print_cmd_struct(supp_cmd->first_cmd);
  }
}

/*-----------------------------------------------------------------*/

void Print_io_def_struct(struct io_def_struct   *io_def)
{
  if (!io_def) return;

  if(io_def->first_io){

    fprintf(stderr, "STEER: IOType definitions:\n");
    Print_io_struct(io_def->first_io);
  }
}

/*-----------------------------------------------------------------*/

void Print_io_struct(struct io_struct   *io)
{
  struct io_struct *ptr   = io;

  while(ptr){

    if(ptr->label)fprintf(stderr, "Label = %s\n",
			  (char *)(ptr->label));
    if(ptr->handle)fprintf(stderr, "Handle = %s\n",
			   (char *)(ptr->handle));
    if(ptr->direction)fprintf(stderr, "Dirn = %s\n",
			      (char *)(ptr->direction));
    if(ptr->freq_handle)fprintf(stderr, "Freq_handle = %s\n",
			       (char *)(ptr->freq_handle));

    ptr = ptr->next;
  }

}

/*-----------------------------------------------------------------*/

void Print_log_struct(struct log_struct *log)
{
  if (!log) return;

  if(log->first_entry){

    fprintf(stderr, "STEER: Log entries:\n");
    Print_log_entry_struct(log->first_entry);
  }
}

/*-----------------------------------------------------------------*/

void Print_log_entry_struct(struct log_entry_struct *entry)
{
  struct log_entry_struct *ptr = entry;
  int                      count = 0;

  while(ptr){

    fprintf(stderr, "STEER: Log entry no. %d:\n", count);

    if(ptr->key) fprintf(stderr, "  Key = %s\n",
			 (char *)ptr->key);

    if(ptr->first_chk_log){
      Print_chk_log_entry_struct(ptr->first_chk_log);
    }
    else if(ptr->first_param_log){
      Print_param_struct(ptr->first_param_log);
    }

    count++;
    ptr = ptr->next;
  }
}

/*-----------------------------------------------------------------*/

void Print_chk_log_entry_struct(struct chk_log_entry_struct *entry)
{
  struct chk_log_entry_struct *ptr = entry;
  int                        count = 0;

  while(ptr){
    fprintf(stderr, "STEER: Chk log entry no. %d:\n", count);

    if(ptr->chk_handle)fprintf(stderr, "  Chk handle = %s\n",
			       (char *)ptr->chk_handle);

    if(ptr->chk_tag)   fprintf(stderr, "  Chk tag = %s\n",
			       (char *)ptr->chk_tag);

    if(ptr->first_param) Print_param_struct(ptr->first_param);

    count++;
    ptr = ptr->next;
  }
}

/*-----------------------------------------------------------------*/

int String_contains_xml_chars(const char *string)
{
  if(strchr(string, '<')){

    return REG_TRUE;
  }
  if(strchr(string, '>')){
    return REG_TRUE;
  }
  if(strchr(string, '&')){
    return REG_TRUE;
  }

  return REG_FALSE;
}

/*-----------------------------------------------------------------*/

/* For handling the xml returned by a findServiceData on a
 * serviceGroupRegistration
 */
int Parse_registry_entries(char* buf, int size,
			   struct registry_contents *contents)
{
  char                  *pChar;
  char                  *pNext;
  char                   entryDelimiter[16];
  xmlDocPtr              doc;
  xmlNodePtr             cur, child, child1;
  int                    numEntries = -1;
  const int              BUFFER_SIZE = 1024;
  int                    maxEntries = 20;
  struct registry_entry *myEntries;
  void                  *pDum;
#ifdef REG_DEBUG_FULL
  int                    i;
#endif

  /* malloc memory to hold content */
  myEntries = (struct registry_entry *)malloc(maxEntries*
					      sizeof(struct registry_entry));
  if( !myEntries ){
    fprintf(stderr, "STEER: Parse_registry_entries: failed to malloc memory\n");
    return REG_FAILURE;
  }

  /* The document we are passed may not have a root element.  Therefore,
     we manually move through the buffer and parse each entry
     separately. */
  pChar = strstr(buf, "<ogsi:entry");
  if(pChar){
    sprintf(entryDelimiter, "<ogsi:entry");
  }
  else{
    pChar = strstr(buf, "<wssg:Entry");
    sprintf(entryDelimiter, "<wssg:Entry");
  }

  /* parse the document */
  while(pChar){
    pNext = strstr(pChar+1, entryDelimiter);
    if(pNext){
      size = (int)(pNext - pChar);
    }
    else{
      /* Look for the closing entry tag - using character [1] instead
       of [0] removes the opening '<' */
      pNext = strstr(pChar+2, &(entryDelimiter[1]));
      pNext += strlen(entryDelimiter);
      size = (int)(pNext - pChar);
      pNext = NULL;
    }

    if( !(doc = xmlParseMemory(pChar, size)) ){
      fprintf(stderr, "STEER: Parse_registry_entries: Hit error parsing "
	      "buffer\n");
      return REG_FAILURE;
    }

    if( !(cur = xmlDocGetRootElement(doc)) ){
      fprintf(stderr,"STEER: Parse_registry_entries: empty document\n");
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
    }

    /* Check that the root node of this xml fragment is what we expect */
    if( xmlStrcmp(cur->name, (const xmlChar *) "Entry") &&
	xmlStrcmp(cur->name, (const xmlChar *) "entry") ){
      fprintf(stderr, "STEER: Parse_registry_entries: root node is not "
	      "an entry\n");
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
    }

    /*
wssg:Entry
  wssg:MemberServiceEPR
    wsa:EndpointReference
      wsa:Address
  wssg:ServiceGroupEntryEPR
    wsa:EndpointReference
      wsa:Address
  wssg:Content
    registryEntry
      serviceType
      componentContent
        componentSoftwarePackage
	componentCreatorName
	componentStartDateTime
	componentTaskDescription
	componentCreatorGroup
      regSecurity
  EntryTerminationTime

ogsi:entry
  ogsi:serviceGroupEntryLocator
    ogsi:locator
      ogsi:handle
  ogsi:memberServiceLocator
    ogsi:locator
      ogsi:handle
  ogsi:content
    registryEntry
     ...
    */
    numEntries++;
    if(numEntries >= maxEntries){
      pDum = realloc(myEntries, 1.5*maxEntries*sizeof(struct registry_entry));
      if(pDum){
	myEntries = (struct registry_entry *)pDum;
	maxEntries = (int)(1.5*maxEntries);
      }
      else{
	fprintf(stderr, "STEER: ERROR: Parse_registry_entries - realloc "
		"failed for entries array\n");
	xmlFreeDoc(doc);
	xmlCleanupParser();
	return REG_FAILURE;
      }
    }
    myEntries[numEntries].pBuf = (char *)malloc(BUFFER_SIZE);
    if(!(myEntries[numEntries].pBuf)){
      fprintf(stderr, "STEER: ERROR: Parse_registry_entries - "
	      "malloc failed\n");
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
    }
    myEntries[numEntries].bufLen = BUFFER_SIZE;
    memset(myEntries[numEntries].pBuf, '\0', BUFFER_SIZE);
    myEntries[numEntries].bufIndex = 0;
    myEntries[numEntries].service_type = NULL;
    myEntries[numEntries].gsh = NULL;
    myEntries[numEntries].application = NULL;
    myEntries[numEntries].start_date_time = NULL;
    myEntries[numEntries].user = NULL;
    myEntries[numEntries].group = NULL;
    myEntries[numEntries].job_description = NULL;

    /* Walk the tree */
    cur = cur->xmlChildrenNode;
    while ( cur ){

      while( xmlIsBlankNode ( cur ) ){
	cur = cur -> next;
      }
      if(!cur)continue;

      /* ogsi - get GSH of registered service */
      if( !xmlStrcmp(cur->name, (const xmlChar *) "memberServiceLocator") ){
	child = cur->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	child = child->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	if( !xmlStrcmp(child->name, (const xmlChar *) "handle") ){
	  Store_xml_string(doc, child, &(myEntries[numEntries].gsh),
			   &(myEntries[numEntries]) );
	}
      }
      else if( !xmlStrcmp(cur->name,
			  (const xmlChar *) "serviceGroupEntryLocator") ){
	child = cur->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	child = child->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	if( !xmlStrcmp(child->name, (const xmlChar *) "handle") ){
	  Store_xml_string(doc, child, &(myEntries[numEntries].entry_gsh),
			   &(myEntries[numEntries]) );
	}
      }
      else if( !xmlStrcmp(cur->name, (const xmlChar *) "MemberServiceEPR") ){
	child = cur->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	child = child->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	if( !xmlStrcmp(child->name, (const xmlChar *) "Address") ){
	  Store_xml_string(doc, child, &(myEntries[numEntries].gsh),
			   &(myEntries[numEntries]) );
	}
      }
      else if( !xmlStrcmp(cur->name,
			  (const xmlChar *) "ServiceGroupEntryEPR") ){
	child = cur->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	child = child->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	if( !xmlStrcmp(child->name, (const xmlChar *) "Address") ){
	  Store_xml_string(doc, child, &(myEntries[numEntries].entry_gsh),
			   &(myEntries[numEntries]) );
	}
      }
      else if( !xmlStrcmp(cur->name,
			  (const xmlChar *) "Content") ||
	       !xmlStrcmp(cur->name,
			  (const xmlChar *) "content") ){
	child = cur->xmlChildrenNode;
	while( xmlIsBlankNode ( child ) ){ /* Find registryEntry node */
	  child = child -> next;
	}
	if(!child){
	  cur = cur->next;
	  continue;
	}
	/* Check whether the content is an xml doc or not */
	child1 = child->xmlChildrenNode;
	if( !child1 ){
	  Store_xml_string(doc, cur, &(myEntries[numEntries].job_description),
			   &(myEntries[numEntries]) );
	}
	child = child1;

	while(child){
	  while( xmlIsBlankNode ( child ) ){
	    child = child -> next;
	  }
	  if(!child) continue;

	  if( !xmlStrcmp(child->name,
			 (const xmlChar *) "serviceType") ){
	    Store_xml_string(doc, child, &(myEntries[numEntries].service_type),
			     &(myEntries[numEntries]) );
	  }
	  else if( !xmlStrcmp(child->name,
			 (const xmlChar *) "componentContent") ){

	    child1 = child->xmlChildrenNode;
	    while(child1){
	      while( xmlIsBlankNode ( child1 ) ){
		child1 = child1 -> next;
	      }
	      if(!child1)continue;

	      if( !xmlStrcmp(child1->name,
			     (const xmlChar *) "componentSoftwarePackage") ){
		Store_xml_string(doc, child1,
				 &(myEntries[numEntries].application),
				 &(myEntries[numEntries]) );
	      }
	      else if( !xmlStrcmp(child1->name,
				  (const xmlChar *) "componentCreatorName") ){
		Store_xml_string(doc, child1,
				 &(myEntries[numEntries].user),
				 &(myEntries[numEntries]) );
	      }
	      else if( !xmlStrcmp(child1->name,
				  (const xmlChar *) "componentStartDateTime") ){
		Store_xml_string(doc, child1,
				 &(myEntries[numEntries].start_date_time),
				 &(myEntries[numEntries]) );
	      }
	      else if( !xmlStrcmp(child1->name,
				  (const xmlChar *) "componentTaskDescription") ){
		Store_xml_string(doc, child1,
				 &(myEntries[numEntries].job_description),
				 &(myEntries[numEntries]) );
	      }
	      else if( !xmlStrcmp(child1->name,
				  (const xmlChar *) "componentCreatorGroup") ){
		Store_xml_string(doc, child1,
				 &(myEntries[numEntries].group),
				 &(myEntries[numEntries]) );
	      }
	      child1 = child1 -> next;
	    } /* while(child1) */
	  }
	  child = child -> next;
	} /* while(child) */
      } /* if node == "Content" */
      cur = cur -> next;
    } /* while(cur) */

    xmlFreeDoc(doc);
    pChar = pNext;
  }
  numEntries++;

  contents->numEntries = numEntries;
  contents->entries = myEntries;

  /* Clean-up */
  xmlCleanupParser();

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Parse_registry_entries: got %d entries "
	  "from registry:\n", numEntries);
  for(i=0; i<numEntries;i++){
    printf("--------------------------------------\n");
    printf("      GSH %02d: %s\n", i, myEntries[i].gsh);
    printf("        app : %s\n", myEntries[i].application);
    printf("   entry gsh: %s\n", myEntries[i].entry_gsh);
    printf("Service type: %s\n", myEntries[i].service_type);
    printf("  Start time: %s\n", myEntries[i].start_date_time);
    printf("        User: %s\n", myEntries[i].user);
    printf("       Group: %s\n", myEntries[i].group);
    printf(" Description: %s\n", myEntries[i].job_description);
  }
#endif

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int Store_xml_string(xmlDocPtr doc, xmlNodePtr cur, char **dest,
		     struct registry_entry *entry){
  int        len, newLen;
  char      *pDum;
  xmlChar   *pXMLChar;
  xmlDocPtr  newDoc = NULL;
  xmlNodePtr tmpNode;
  /* int        i; for debug output */
  long       ptrShift;

  tmpNode = cur->xmlChildrenNode;
  while( xmlIsBlankNode(tmpNode) ){
    tmpNode = tmpNode->next;
  }
  if(!tmpNode) return REG_FAILURE;

  if(tmpNode->type == XML_TEXT_NODE){
    /* Content is just text so grab it... */
    pXMLChar = xmlNodeListGetString(doc, tmpNode, 1);
    len = xmlStrlen(pXMLChar);
  }
  else{
    /* Otherwise we have xml so convert it back to text... */
    newDoc = xmlNewDoc(BAD_CAST "1.0");
    xmlDocSetRootElement(newDoc, tmpNode);
    xmlDocDumpFormatMemory(newDoc, &pXMLChar, &len, 1);
  }

  /* '-1' allows for terminating '\0' */
  if(entry->bufLen - entry->bufIndex - len - 1 <= 0){
    newLen = (entry->bufLen + 2*len);
    if( (pDum = (char*)realloc((void*)(entry->pBuf), newLen)) ){
      ptrShift = pDum - entry->pBuf;
      entry->pBuf = pDum;
      entry->bufLen = newLen;
      /* Need to update other stored pointers...*/
      if(entry->application) entry->application += ptrShift;
      if(entry->gsh) entry->gsh += ptrShift;
      if(entry->entry_gsh) entry->entry_gsh += ptrShift;
      if(entry->user) entry->user += ptrShift;
      if(entry->group) entry->group += ptrShift;
      if(entry->job_description) entry->job_description += ptrShift;
      if(entry->service_type) entry->service_type += ptrShift;
      if(entry->start_date_time) entry->start_date_time += ptrShift;
    }
    else{
      fprintf(stderr, "STEER: ERROR: Store_xml_string - realloc failed\n");
      return REG_FAILURE;
    }
  }
  /*
  printf("ARPDBG: pBuf currently holds >>");
  for(i=0;i<entry->bufIndex;i++){
    printf("%c", entry->pBuf[i]);
  }
  printf("<<\n");
  */
  *dest = &(entry->pBuf[entry->bufIndex]);
  strncpy(*dest, (char*) pXMLChar, len);
  (*dest)[len] = '\0';
  entry->bufIndex += len + 1;
  xmlFree(pXMLChar);
  if (newDoc) xmlFreeDoc(newDoc);

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
/** Check to see whether or not this message has been seen within the
    last storeSize messages received
    @internal */
int Msg_already_received(char *msg_uid,
			 struct msg_uid_history_struct *hist)
{
  int                  i;
  unsigned int         uid;

  if (!msg_uid) return REG_FALSE;

  if(!(hist->uidStorePtr)){
    for(i=0; i<REG_UID_HISTORY_BUFFER_SIZE; i++){
      /* Initialize array with a value not used as a msg UID */
      hist->uidStore[i] = -1;
    }
    hist->uidStorePtr = hist->uidStore;
    hist->maxPtr = &(hist->uidStore[REG_UID_HISTORY_BUFFER_SIZE-1]);
  }

  if(sscanf(msg_uid, "%u", &uid) != 1){
    fprintf(stderr, "STEER ERROR: Msg_already_received: failed to "
	    "read msg UID\n");
    return REG_FALSE;
  }

  for(i=0; i<REG_UID_HISTORY_BUFFER_SIZE; i++){
    /* Have seen this uid before so return & don't alter our records */
    if(hist->uidStore[i] == uid)return REG_TRUE;
  }
  /* We haven't seen it before so store it */
  *(hist->uidStorePtr) = uid;
  if(hist->uidStorePtr == hist->maxPtr){
    /* Wrap ptr back to beginning of array */
    hist->uidStorePtr = hist->uidStore;
  }
  else{
    hist->uidStorePtr++;
  }

  return REG_FALSE;
}

/*------------------------------------------------------------------*/

int Delete_msg_store(struct msg_store_struct *msgStore)
{
  struct msg_store_struct *curEntry;
  struct msg_store_struct *tmp;

  if(!msgStore)return REG_FAILURE;

  Delete_msg_struct(&(msgStore->msg));
  tmp = msgStore->next;
  while( (curEntry = tmp) ){
    Delete_msg_struct(&(curEntry->msg));
    tmp = curEntry->next;
    free(curEntry);
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/

int Delete_msg_uid_store(struct msg_uid_history_struct *uidHist)
{
  if(!uidHist)return REG_FAILURE;

  /* No actual free'ing to do - just reset pointers */
  uidHist->uidStorePtr = NULL;
  uidHist->maxPtr = NULL;
  return REG_SUCCESS;
}
