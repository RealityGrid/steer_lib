/*----------------------------------------------------------------------------
  This file contains routines and data structures for parsing the XML
  steering-communication messages

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

#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "ReG_Steer_Browser.h"
#include "ReG_Steer_Steerside.h"

#include <string.h>

#ifndef REG_DEBUG
#define REG_DEBUG 1
#endif

/** @file ReG_Steer_XML.c
 *  @author Andrew Porter
 *  @brief Code for parsing xml documents */

/** Declared in ReG_Steer_Appside.c */
extern struct msg_store_struct  Msg_store;
extern struct msg_store_struct *Msg_store_tail;

/** Declared in ReG_Steer_Appside.c */
extern struct msg_uid_history_struct Msg_uid_store;

/*-----------------------------------------------------------------*/

int Parse_xml_file(char* filename, struct msg_struct *msg,
		   Sim_entry_type *sim)
{ 
  xmlDocPtr doc;

  doc = xmlParseFile(filename);

  if (doc == NULL){
    fprintf(stderr, "Parse_xml_file: Hit error parsing file %s\n", filename);
    return REG_FAILURE;
  }

  return Parse_xml(doc, msg, sim);
}

/*-----------------------------------------------------------------*/

int Parse_xml_buf(char* buf, int size, struct msg_struct *msg,
		  Sim_entry_type *sim)
{ 
  xmlDocPtr doc;

  fprintf(stderr, "CALLTREE: Parse_xml_buf\n");
  if(!buf){

    fprintf(stderr, "Parse_xml_buf: ptr to buffer is NULL\n");
    return REG_FAILURE;
  }

  doc = xmlParseMemory(buf, size);

  if (doc == NULL){
    fprintf(stderr, "Parse_xml_buf: Hit error parsing buffer\n");
    return REG_FAILURE;
  }

  return Parse_xml(doc, msg, sim);
}

/*-----------------------------------------------------------------*/

int Parse_xml(xmlDocPtr doc, struct msg_struct *msg,  
	      Sim_entry_type *sim)
{
  xmlNsPtr   ns;
  xmlNodePtr cur;
#if REG_DEBUG_FULL
  struct msg_store_struct *cur_msg;
#endif
  fprintf(stderr, "CALLTREE: Parse_xml\n");

  cur = xmlDocGetRootElement(doc);
  if (cur == NULL) {
      fprintf(stderr,"Parse_xml: empty document\n");
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
  }

  ns = xmlSearchNsByHref(doc, cur,
            (const xmlChar *) "http://www.realitygrid.org/xml/steering");
  /* Relax our conditions to generalize this parser------------
  if (ns == NULL) {
      fprintf(stderr,
              "Parse_xml: document of the wrong type, ReG namespace not found\n");
      xmlFreeDoc(doc);
      return REG_FAILURE;
  }
  -----------------------*/

  if (!xmlStrcmp(cur->name, (const xmlChar *) "ReG_steer_message")) {
#if REG_DEBUG_FULL
    fprintf(stderr,"Parse_xml: Have ReG_steer_message doc\n");
#endif

    if(parseSteerMessage(doc, ns, cur, msg, sim) != REG_SUCCESS){
      xmlFreeDoc(doc);
      xmlCleanupParser();
      return REG_FAILURE;
    }

    /* Print out what we've got */
#if REG_DEBUG_FULL
    if(msg && msg->msg_type != MSG_NOTSET){
      fprintf(stderr, "Parse_xml: Calling Print_msg...\n");
      Print_msg(msg);
    }
#endif
  }
  else if (!xmlStrcmp(cur->name, (const xmlChar *) "ResourceProperties") ||
	   !xmlStrcmp(cur->name, (const xmlChar *) "controlMsg")) {
#if REG_DEBUG_FULL
    fprintf(stderr,"Parse_xml: passing %s to parseResourceProperties...\n",
	    (char *)(cur->name));
#endif
    parseResourceProperties(doc, ns, cur, sim);

    /* Print out what we've got */
#if REG_DEBUG_FULL
    if(sim){
      cur_msg = &(sim->Msg_store);
      while(cur_msg){
	fprintf(stderr, "Parse_xml: Calling Print_msg...\n");
	if(cur_msg->msg)Print_msg(cur_msg->msg);
	cur_msg = cur_msg->next;
      }
    }
#endif
  }
  else{
    fprintf(stderr,"Parse_xml: document of the wrong type, root node "
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

  fprintf(stderr, "CALLTREE: Extract_resource_property for %s\n",
	  name);

  if(!pRPDoc){

    fprintf(stderr, "STEER: Extract_resource_property: ptr to RP "
	    "document is NULL\n");
    return REG_FAILURE;
  }

  if( !(pStart = strstr(pRPDoc, name)) ){
    fprintf(stderr, "STEER: Extract_resource_property: RP %s not found\n",
	    name);
    return REG_FAILURE;
  }
  /* Move ptr forwards to point to beginning of content - allow for closing
     angle bracket */
  pStart = strchr(pStart, '>');
  pStart++;

  if(!(pStop = strstr(pStart, name))){
    fprintf(stderr, "STEER: Extract_resource_property: closing tag for "
	    "RP %s not found\n", name);
    return REG_FAILURE;
  }

  /* Copy the contents into our result buffer - -2 is to allow for '</' of
     closing element tag */
  len = (int)(pStop - 2 - pStart);
  strncpy(resultBuf, pStart, len);
  resultBuf[len] = '\0';

#if REG_DEBUG_FULL
  /*
  fprintf(stderr, "STEER: Extract_resource_property: Value of RP "
	  "%s = >>%s<<\n", name, resultBuf);
  */
#endif
  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseResourceProperties(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
			    Sim_entry_type *sim)
{
  xmlNodePtr child;
  xmlChar   *steerStatus;
  struct msg_store_struct *curMsg;

  fprintf(stderr, "CALLTREE: parseResourceProperties\n");

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
#if REG_DEBUG_FULL
	fprintf(stderr, "STEER: parseResourceProperties: Calling "
		"parseSteerMessage...\n");
#endif
	curMsg->msg = New_msg_struct();

	if(parseSteerMessage(doc, ns, child, curMsg->msg, sim) !=
	   REG_SUCCESS){
	  Delete_msg_struct(&(curMsg->msg));
	}
	else{
	  fprintf(stderr, "ARPDBG STORING msg with UID %s\n", 
		  curMsg->msg->msg_uid);
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
    else{
      fprintf(stderr, "STEER: parseResourceProperties: ignoring node: %s\n", 
	      (char *)(cur->name));
    }

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

int parseSteerMessage(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		      struct msg_struct *msg, Sim_entry_type *sim)
{
  struct msg_uid_history_struct *uidStorePtr;

  fprintf(stderr, "CALLTREE: parseSteerMessage\n");

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
  if( (msg->msg_uid = xmlGetProp(cur, "Msg_UID")) ){
#if REG_DEBUG_FULL
    fprintf(stderr, "parseSteerMessage: msg UID = %s\n",
	    (char*)(msg->msg_uid));
#endif
    /* Check that we haven't already seen this message 
       before we bother to store it */
    if( Msg_already_received((char*)(msg->msg_uid), uidStorePtr) ){
#if REG_DEBUG
      fprintf(stderr, "STEER: INFO: parseSteerMessage: msg"
	      " with UID %s has been seen before\n", 
	      msg->msg_uid);
#endif
      /* We have - skip this one */
      msg->msg_type = MSG_NOTSET;
      return REG_FAILURE;
    }
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
#if REG_DEBUG_FULL
    fprintf(stderr, "parseSteerMessage: Calling parseStatus...\n");
#endif
    msg->status = New_status_struct();
    parseStatus(doc, ns, cur, msg->status);
    break;

  case CONTROL:
#if REG_DEBUG_FULL
    fprintf(stderr, "parseSteerMessage: Calling parseControl...\n");
#endif
    msg->control = New_control_struct();
    parseControl(doc, ns, cur, msg->control);
    break;

  case SUPP_CMDS:
#if REG_DEBUG_FULL
    fprintf(stderr, "parseSteerMessage: Calling parseSuppCmd...\n");
#endif
    msg->supp_cmd = New_supp_cmd_struct();
    parseSuppCmd(doc, ns, cur, msg->supp_cmd);
    break;

  case PARAM_DEFS:
#if REG_DEBUG_FULL
    fprintf(stderr, "parseSteerMessage: Calling parseStatus...\n");
#endif
    /* Use code for 'status' messages because one 
       encapsulates the other */
    msg->status = New_status_struct();
    parseStatus(doc, ns, cur, msg->status);
    break;

  case IO_DEFS:
#if REG_DEBUG_FULL
    fprintf(stderr, "parseSteerMessage: Calling parseIOTypeDef...\n");
    fprintf(stderr, "parseSteerMessage: ARPDBG, msg ptr = %p\n", msg);
#endif
    msg->io_def = New_io_def_struct();
    parseIOTypeDef(doc, ns, cur, msg->io_def);
    break;

  case CHK_DEFS:
#if REG_DEBUG_FULL
    fprintf(stderr, "Parse_xml: Calling parseChkTypeDef...\n");
#endif
    msg->chk_def = New_io_def_struct();
    parseChkTypeDef(doc, ns, cur, msg->chk_def);
    break;

  case STEER_LOG:
#if REG_DEBUG_FULL
    fprintf(stderr, "Parse_xml: Calling parseLog...\n");
#endif
    msg->log = New_log_struct();
    parseLog(doc, ns, cur, msg->log);
    break;

  default:
    fprintf(stderr, "Parse_xml: Unrecognised message type %d for "
	    "message name >>%s<<\n",
	    msg->msg_type, (const char *)(cur->name));
    break;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseStatus(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	        struct status_struct *status)
{

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

#if REG_DEBUG
      fprintf(stderr, "Calling parseParam...\n");
#endif
      parseParam(doc, ns, cur, status->param);
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

#if REG_DEBUG
      fprintf(stderr, "Calling parseCmd...\n");
#endif
      parseCmd(doc, ns, cur, status->cmd);
    }

    cur = cur->next;
  } 
  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseControl(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	         struct control_struct *ctrl)
{
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

#if REG_DEBUG
      fprintf(stderr, "Calling parseParam...\n");
#endif
      parseParam(doc, ns, cur, ctrl->param);
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

#if REG_DEBUG
      fprintf(stderr, "Calling parseCmd...\n");
#endif
      parseCmd(doc, ns, cur, ctrl->cmd);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseIOTypeDef(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		   struct io_def_struct *io_def)
{

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

#if REG_DEBUG
      fprintf(stderr, "parseIOTypeDef: Calling parseIOType...\n");
#endif
      parseIOType(doc, ns, cur, io_def->io);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseChkTypeDef(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		   struct io_def_struct *chk_def)
{

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

#if REG_DEBUG
      fprintf(stderr, "parseChkTypeDef: Calling parseIOType...\n");
#endif
      parseIOType(doc, ns, cur, chk_def->io);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseIOType(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		 struct io_struct *io)
{
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

int parseSuppCmd(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		 struct supp_cmd_struct *supp_cmd)
{
  if(supp_cmd == NULL){
    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;
  while(cur != NULL){

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Command") ){
      /*&& (cur->ns == ns)) {*/

      if( !supp_cmd->first_cmd ){

	supp_cmd->first_cmd = New_cmd_struct();
	supp_cmd->cmd = supp_cmd->first_cmd;
      }
      else{
	supp_cmd->cmd->next = New_cmd_struct();
	supp_cmd->cmd = supp_cmd->cmd->next;
      }

#if REG_DEBUG_FULL
      fprintf(stderr, "parseSuppCmd: Calling parseCmd...\n");
#endif
      parseCmd(doc, ns, cur, supp_cmd->cmd);
    }
#if REG_DEBUG
    else{
      fprintf(stderr, "parseSuppCmd: name = %s <> Command\n", cur->name);
    }
#endif

    cur = cur->next;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseLog(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	          struct log_struct *log)
{
  if (!log) return REG_FAILURE;

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *)"Log_entry") ){
      /*&& (cur->ns==ns)){*/

      if(!log->first_entry){

	log->first_entry = New_log_entry_struct();
	log->entry = log->first_entry;
      }
      else{
	log->entry->next = New_log_entry_struct();
	log->entry = log->entry->next;
      }

#if REG_DEBUG
      fprintf(stderr, "parseLog: calling parseLogEntry\n");
#endif
      parseLogEntry(doc, ns, cur, log->entry);

    }
    cur = cur->next;
  }
  
  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int parseLogEntry(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	               struct log_entry_struct *log)
{
  int return_status = REG_SUCCESS;

  if(!log) return REG_FAILURE;

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {
  
    if( !xmlStrcmp(cur->name, (const xmlChar *)"Key") ){
      /*&& (cur->ns==ns)){*/

      log->key = xmlNodeListGetString(doc, 
				      cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *)"Chk_log_entry") ){
      /*	     && (cur->ns==ns)){*/
      if(!log->first_chk_log){
	
	log->first_chk_log = New_chk_log_entry_struct();
	log->chk_log = log->first_chk_log;
      }
      else{
	log->chk_log->next = New_chk_log_entry_struct();
	log->chk_log = log->chk_log->next;
      }
#if REG_DEBUG
      fprintf(stderr, "parseLogEntry: calling parseChkLogEntry\n");
#endif
      return_status = parseChkLogEntry(doc, ns, cur, log->chk_log);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *)"Param") ){
      /*	     && (cur->ns==ns)){*/
      if(!log->first_param_log){

	log->first_param_log = New_param_struct();
	log->param_log = log->first_param_log;
      }
      else{
	log->param_log->next = New_param_struct();
	log->param_log = log->param_log->next;
      }

#if REG_DEBUG
      fprintf(stderr, "parseLogEntry: calling parseParam\n");
#endif
      return_status = parseParam(doc, ns, cur, log->param_log);
    }

    cur = cur->next;
  }
  return return_status;
}

/*-----------------------------------------------------------------*/

int parseChkLogEntry(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
		     struct chk_log_entry_struct *log_entry)
{
  int return_status = REG_SUCCESS;

  if(!log_entry) return REG_FAILURE;

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if(!xmlStrcmp(cur->name, (const xmlChar *)"Chk_handle") ){
      /*&& (cur->ns==ns)){*/

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

      return_status = parseParam(doc, ns, cur, log_entry->param);
    }
    
    cur = cur->next;
  }

  return return_status;
}

/*-----------------------------------------------------------------*/

int parseParam(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	       struct param_struct *param)
{
  if(param == NULL){

    return REG_FAILURE;
  }

  cur = cur->xmlChildrenNode;
  while (cur != NULL) {

    if( !xmlStrcmp(cur->name, (const xmlChar *)"Handle") ){
      /*&& (cur->ns==ns)) {*/

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

int parseCmd(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur,
	     struct cmd_struct *cmd)
{
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

      parseParam(doc, ns, cur, cmd->param);
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

    fprintf(stderr, "Print_msg: ptr to msg struct is null\n");
    return;
  }

  if(msg->msg_type == MSG_ERROR){
    fprintf(stderr, "Print_msg: msg is ERROR message\n");    
  }
  else if(msg->msg_type == MSG_NOTSET){
    fprintf(stderr, "Print_msg: msg type is unset\n");
  }
  else if(msg->msg_type == SUPP_CMDS){
    fprintf(stderr, "Print_msg: msg type is SUPP_CMDS\n");
  }
  else if(msg->msg_type == IO_DEFS){
    fprintf(stderr, "Print_msg: msg type is IO_DEFS\n");
  }
  else if(msg->msg_type == PARAM_DEFS){
    fprintf(stderr, "Print_msg: msg type is PARAM_DEFS\n");
  }
  else if(msg->msg_type == STATUS){
    fprintf(stderr, "Print_msg: msg type is STATUS\n");
  }
  else if(msg->msg_type == CONTROL){
    fprintf(stderr, "Print_msg: msg type is CONTROL\n");
  }
  else if(msg->msg_type == CHK_DEFS){
    fprintf(stderr, "Print_msg: msg type is CHK_DEFS\n");
  }
  else if(msg->msg_type == STEER_LOG){
    fprintf(stderr, "Print_msg: msg type is STEER_LOG\n");
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

  fprintf(stderr, "Print_msg: done\n");
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
    fprintf(stderr, "Param no. %d:\n", count);
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
    fprintf(stderr, "Command no. %d:\n", count);
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

    fprintf(stderr, "Supported commands:\n");
    Print_cmd_struct(supp_cmd->first_cmd);
  }
}

/*-----------------------------------------------------------------*/

void Print_io_def_struct(struct io_def_struct   *io_def)
{
  if (!io_def) return;

  if(io_def->first_io){

    fprintf(stderr, "IOType definitions:\n");
    Print_io_struct(io_def->first_io);
  }
}

/*-----------------------------------------------------------------*/

void Print_io_struct(struct io_struct   *io)
{
  struct io_struct *ptr   = io;
  int               count = 0;

  while(ptr){

    fprintf(stderr, "IO def. no.: %d\n", count++);
    fprintf(stderr, "ARPDBG, ptr to io struct = %p\n", ptr);

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

    fprintf(stderr, "Log entries:\n");
    Print_log_entry_struct(log->first_entry);
  }
}

/*-----------------------------------------------------------------*/

void Print_log_entry_struct(struct log_entry_struct *entry)
{
  struct log_entry_struct *ptr = entry;
  int                      count = 0;

  while(ptr){

    fprintf(stderr, "Log entry no. %d:\n", count);

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
    fprintf(stderr, "Chk log entry no. %d:\n", count);

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

int String_contains_xml_chars(char *string)
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
 * serviceGroupRegistration - uses SAX rather than DOM.
 */
int Parse_registry_entries(char* buf, int size, int *num_entries, 
			   struct registry_entry **entries)
{ 
  static xmlSAXHandler my_handler;
  struct ParserState my_state;
#if REG_DEBUG
  int i;
#endif

  /* Set pointers to our event handlers */
  my_handler.startElement = Start_element_handler;
  my_handler.endElement = End_element_handler;
  my_handler.characters = Characters_handler;

  /* Initialise the structure that holds the state of our parser */
  my_state.num_entries = 0;
  my_state.max_entries = 15;
  my_state.depth = STARTING;
  my_state.return_val = REG_SUCCESS;

  /* malloc memory to hold content */
  my_state.entries = (struct registry_entry *)malloc(my_state.max_entries*
					     sizeof(struct registry_entry));
  if(!(my_state.entries)){
    fprintf(stderr, "Parse_registry_entries: failed to malloc memory\n");
    return REG_FAILURE;
  }

  /* parse the document */
  if (xmlSAXUserParseMemory(&my_handler, &my_state, buf, size) < 0) {

    free(my_state.entries);
    my_state.entries = NULL;
    return REG_FAILURE;
  } 
  /* Clean-up */
  xmlCleanupParser();

  *num_entries = my_state.num_entries;
  *entries = my_state.entries;

#if REG_DEBUG
  fprintf(stderr, "Got %d entries from registry:\n", my_state.num_entries);
  for(i=0; i<my_state.num_entries;i++){

    printf("GSH  %d: %s\n", i, my_state.entries[i].gsh);
    printf("app   : %s\n", my_state.entries[i].application);
  }
#endif

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

void Start_element_handler(void * 	user_data,
			   const xmlChar * 	name,
			   const xmlChar ** 	attrs){

  struct ParserState *state;
  state = (struct ParserState *)user_data;

  /* Check that we haven't previously hit an error */
  if(state->return_val != REG_SUCCESS) return;
  /*
  wssg:Entry
  wssg:MemberServiceEPR
  <EndpointReference>
  <Address></Address>
  </EndpointReference>
  /wssg:MemberServiceEPR
  ...
  wssg:Content
  <registryEntry>
  ...
  </registryEntry>
  /wssg:Content
  */
  fprintf(stderr, "Start_element_handler: name = %s\n", (char *)name);

  if( !xmlStrcmp(name, (const xmlChar *) "ogsi:entry") ){

    if (state->depth == STARTING){
      state->depth = OGSI_ENTRY;
      /* Initialise table to hold this content */
      state->entries[state->num_entries].service_type[0] = '\0';
      state->entries[state->num_entries].gsh[0] = '\0';
      state->entries[state->num_entries].application[0] = '\0';
      state->entries[state->num_entries].start_date_time[0] = '\0';
      state->entries[state->num_entries].user[0] = '\0';
      state->entries[state->num_entries].group[0] = '\0';
      state->entries[state->num_entries].job_description[0] = '\0';
    }
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "wssg:Entry") ){

    if (state->depth == STARTING){
      state->depth = WSRF_ENTRY;
      fprintf(stderr, "ARPDBG depth = WSRF_ENTRY\n");
      /* Initialise table to hold this content */
      state->entries[state->num_entries].service_type[0] = '\0';
      state->entries[state->num_entries].gsh[0] = '\0';
      state->entries[state->num_entries].application[0] = '\0';
      state->entries[state->num_entries].start_date_time[0] = '\0';
      state->entries[state->num_entries].user[0] = '\0';
      state->entries[state->num_entries].group[0] = '\0';
      state->entries[state->num_entries].job_description[0] = '\0';
    }
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "ogsi:memberServiceLocator") ){

    if (state->depth == OGSI_ENTRY) state->depth=MEMBER_SERVICE_LOCATOR;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "ogsi:handle") ){

    if(state->depth == MEMBER_SERVICE_LOCATOR) state->depth = GS_HANDLE;
  } 
  else if( !xmlStrcmp(name, (const xmlChar *) "ogsi:content") ){

    if(state->depth == OGSI_ENTRY) state->depth = CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "serviceType") ){

    if(state->depth == CONTENT) state->depth = SERVICE_TYPE;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentContent") ){

    if(state->depth == CONTENT) state->depth = COMPONENT_CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentStartDateTime") ){

    if(state->depth == COMPONENT_CONTENT) state->depth = COMPONENT_START_DATE_TIME;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentCreatorName") ){

    if(state->depth == COMPONENT_CONTENT) state->depth = COMPONENT_CREATOR_NAME;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentCreatorGroup") ){

    if(state->depth == COMPONENT_CONTENT) state->depth = COMPONENT_CREATOR_GROUP;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentSoftwarePackage") ){

    if(state->depth == COMPONENT_CONTENT) state->depth = COMPONENT_SOFTWARE_PACKAGE;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentTaskDescription") ){

    if(state->depth == COMPONENT_CONTENT) state->depth = COMPONENT_TASK_DESCRIPTION;
  }
  /* WSRF section */
  else if( !xmlStrcmp(name, (const xmlChar *) "wssg:MemberServiceEPR") ){
    if(state->depth == WSRF_ENTRY){
      state->depth = MEMBER_SERVICE_EPR;
      fprintf(stderr, "ARPDBG depth = MEMBER_SERVICE_EPR\n");
    }
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "EndpointReference") ){
    if(state->depth == MEMBER_SERVICE_EPR){
      state->depth = EPR;
      fprintf(stderr, "ARPDBG depth = EPR\n");
    }
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "Address") ){
    if(state->depth == EPR){
      state->depth = WSADDRESS;
       fprintf(stderr, "ARPDBG depth = WSADDRESS\n");
   }
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "wssg:Content") ){
    if(state->depth == WSRF_ENTRY){
      state->depth = CONTENT;
      fprintf(stderr, "ARPDBG depth = CONTENT\n");
    }
  }
}

/*-----------------------------------------------------------------*/

void End_element_handler(void          *user_data,
			 const xmlChar *name){

  struct ParserState    *state;
  struct registry_entry *tmp;

  state = (struct ParserState *)user_data;

  /* Check that we haven't previously hit an error */
  if(state->return_val != REG_SUCCESS) return;

  if( !xmlStrcmp(name, (const xmlChar *) "ogsi:entry") ){

    if(state->depth == OGSI_ENTRY){
      state->depth = STARTING;

      /* malloc more memory if required */  
      if(++(state->num_entries) >= state->max_entries){

	tmp = realloc(state->entries,
		      2*(state->max_entries)*sizeof(struct registry_entry));
	if(tmp){
	  state->entries = tmp;
	  state->max_entries *= 2;
#if REG_DEBUG_FULL
	  fprintf(stderr, "INFO: End_element_handler: done malloc for "
		  "%d entries\n", state->max_entries);
#endif
	}
	else{
	  state->return_val = REG_FAILURE;
	}
      }
    }
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "ogsi:memberServiceLocator") ){

    if(state->depth == MEMBER_SERVICE_LOCATOR) state->depth = OGSI_ENTRY;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "ogsi:handle") ){

    if (state->depth == GS_HANDLE) state->depth = MEMBER_SERVICE_LOCATOR;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "ogsi:content") ){

    if (state->depth == CONTENT) state->depth = OGSI_ENTRY;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "serviceType") ){

    if(state->depth == SERVICE_TYPE) state->depth = CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentContent") ){

    if(state->depth == COMPONENT_CONTENT) state->depth = CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentStartDateTime") ){

    if(state->depth == COMPONENT_START_DATE_TIME) state->depth = COMPONENT_CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentCreatorName") ){

    if(state->depth == COMPONENT_CREATOR_NAME) state->depth =  COMPONENT_CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentCreatorGroup") ){

    if(state->depth == COMPONENT_CREATOR_GROUP) state->depth = COMPONENT_CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentSoftwarePackage") ){

    if(state->depth == COMPONENT_SOFTWARE_PACKAGE) state->depth = COMPONENT_CONTENT;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "componentTaskDescription") ){

    if(state->depth == COMPONENT_TASK_DESCRIPTION) state->depth = COMPONENT_CONTENT;
  }
  /* WSRF section */
  else if( !xmlStrcmp(name, (const xmlChar *) "wssg:Entry") ){

    if(state->depth == WSRF_ENTRY){
      state->depth = STARTING;

      /* malloc more memory if required */  
      if(++(state->num_entries) >= state->max_entries){

	tmp = realloc(state->entries,
		      2*(state->max_entries)*sizeof(struct registry_entry));
	if(tmp){
	  state->entries = tmp;
	  state->max_entries *= 2;
#if REG_DEBUG_FULL
	  fprintf(stderr, "INFO: End_element_handler: done malloc for "
		  "%d entries\n", state->max_entries);
#endif
	}
	else{
	  state->return_val = REG_FAILURE;
	}
      }
    }
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "wssg:MemberServiceEPR") ){
    if(state->depth == MEMBER_SERVICE_EPR) state->depth = WSRF_ENTRY;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "EndpointReference") ){
    if(state->depth == EPR) state->depth = MEMBER_SERVICE_EPR;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "Address") ){
    if(state->depth == WSADDRESS) state->depth = EPR;
  }
  else if( !xmlStrcmp(name, (const xmlChar *) "wssg:Content") ){
    if(state->depth == CONTENT) state->depth = WSRF_ENTRY;
  }
}

/*-----------------------------------------------------------------*/

void Characters_handler(void          *user_data,
			const xmlChar *ch,
			int  	       len){

  struct ParserState *state;
  state = (struct ParserState *)user_data;

  /* Check that we haven't previously hit an error */
  if(state->return_val != REG_SUCCESS) return;

  if(state->depth == GS_HANDLE || state->depth == WSADDRESS){
    strncpy(state->entries[state->num_entries].gsh, (char *)ch, len);
    state->entries[state->num_entries].gsh[len] = '\0';
  } 
  else if (state->depth == SERVICE_TYPE){
    strncpy(state->entries[state->num_entries].service_type, (char *)ch, len);
    state->entries[state->num_entries].service_type[len]='\0';
  }
  else if (state->depth == COMPONENT_START_DATE_TIME){
    strncpy(state->entries[state->num_entries].start_date_time, (char *)ch, len);
    state->entries[state->num_entries].start_date_time[len] = '\0';
  }
  else if (state->depth == COMPONENT_CREATOR_NAME){
    strncpy(state->entries[state->num_entries].user, (char *)ch, len);
    state->entries[state->num_entries].user[len] = '\0';
  }
  else if (state->depth == COMPONENT_CREATOR_GROUP){
    strncpy(state->entries[state->num_entries].group, (char *)ch, len);
    state->entries[state->num_entries].group[len] = '\0';
  }
  else if (state->depth == COMPONENT_SOFTWARE_PACKAGE){
    strncpy(state->entries[state->num_entries].application, (char *)ch, len);
    state->entries[state->num_entries].application[len] = '\0';
  }
  else if (state->depth == COMPONENT_TASK_DESCRIPTION){
    strncpy(state->entries[state->num_entries].job_description, (char *)ch, len);
    state->entries[state->num_entries].job_description[len] = '\0';
  }
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

  if(msg_uid){
 
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
      if(hist->uidStore[i] == uid)return REG_TRUE;
    }
    *(hist->uidStorePtr) = uid;
    if(hist->uidStorePtr == hist->maxPtr){
      /* Wrap ptr back to beginning of array */
      hist->uidStorePtr = hist->uidStore;
    }
    else{
      hist->uidStorePtr++;
    }

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
