/*----------------------------------------------------------------------------
    This file contains routines and data structures for parsing the XML
    steering-communication messages

    FILE-BASED implementation.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the authors, nor the University of
    Manchester offer any warranties or representations, nor do they
    accept any liabilities with respect to this software.

    This program must not be used for commmercial gain without the
    written permission of the authors.
    
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL

    email:  csar-advice@cfs.ac.uk.
    Tel:    +44 161 275 6824/5997
    Fax:    +44 161 275 6040    
    
    Date          Version    Updates                            Author
    ----          -------    -------                            ------
    24.9.2002       0.1                                         A Porter

---------------------------------------------------------------------------*/

#include "ReG_Steer_types.h"
#include "ReG_Steer_XML.h"

#ifndef DEBUG
#define DEBUG 1
#endif

/*-----------------------------------------------------------------*/

int Parse_xml_file(char* filename, struct msg_struct *msg)
{ 
  xmlDocPtr doc;

  doc = xmlParseFile(filename);

  if (doc == NULL){
    fprintf(stderr, "Parse_xml_file: Hit error parsing file %s\n", filename);
    return REG_FAILURE;
  }

  return Parse_xml(doc, msg);
}

/*-----------------------------------------------------------------*/

int Parse_xml_buf(char* buf, int size, struct msg_struct *msg)
{ 
  xmlDocPtr doc;

  if(buf == NULL){

    fprintf(stderr, "Parse_xml_buf: ptr to buffer is NULL\n");
    return REG_FAILURE;
  }

  doc = xmlParseMemory(buf, size);

  if (doc == NULL){
    fprintf(stderr, "Parse_xml_buf: Hit error parsing buffer\n");
    return REG_FAILURE;
  }

  return Parse_xml(doc, msg);
}

/*-----------------------------------------------------------------*/

int Parse_xml(xmlDocPtr doc, struct msg_struct *msg)
{
  xmlNsPtr   ns;
  xmlNodePtr cur;

  cur = xmlDocGetRootElement(doc);
  if (cur == NULL) {
      fprintf(stderr,"empty document\n");
      xmlFreeDoc(doc);
      return REG_FAILURE;
  }
  
  ns = xmlSearchNsByHref(doc, cur,
            (const xmlChar *) "http://www.realitygrid.org/xml/steering");
  if (ns == NULL) {
      fprintf(stderr,
              "document of the wrong type, ReG namespace not found\n");
      xmlFreeDoc(doc);
      return REG_FAILURE;
  }

  if (xmlStrcmp(cur->name, (const xmlChar *) "ReG_steer_message")) {
      fprintf(stderr,"document of the wrong type, root node != "
	      "ReG_steer_message");
      xmlFreeDoc(doc);
      return REG_FAILURE;
  }


  /* Now, walk the tree */
  cur = cur->xmlChildrenNode;
  while ( cur && xmlIsBlankNode ( cur ) ){

        cur = cur -> next;
  }

  if ( cur == 0 )return REG_FAILURE;


  if( !xmlStrcmp(cur->name, (const xmlChar *) "App_status") ){

    /* Record the message type */
    msg->msg_type = Get_message_type((const char *)(cur->name));

#if DEBUG
    fprintf(stderr, "Calling parseStatus...\n");
#endif
    msg->status = New_status_struct();
    parseStatus(doc, ns, cur, msg->status);
  }
  else if( !xmlStrcmp(cur->name, (const xmlChar *) "Steer_control") ){

    /* Record the message type */
    msg->msg_type = Get_message_type((const char *)(cur->name));

#if DEBUG
    fprintf(stderr, "Calling parseControl...\n");
#endif
    msg->control = New_control_struct();
    parseControl(doc, ns, cur, msg->control);
  }
  else if( !xmlStrcmp(cur->name, (const xmlChar *) "Supported_commands") ){

    /* Record the message type */
    msg->msg_type = Get_message_type((const char *)(cur->name));

#if DEBUG
    fprintf(stderr, "Calling parseSuppCmd...\n");
#endif
    msg->supp_cmd = New_supp_cmd_struct();
    parseSuppCmd(doc, ns, cur, msg->supp_cmd);
  }
  else if( !xmlStrcmp(cur->name, (const xmlChar *) "Param_defs") ){

    /* Record the message type */
    msg->msg_type = Get_message_type((const char *)(cur->name));

#if DEBUG
    fprintf(stderr, "Calling parseStatus...\n");
#endif
    /* Use code for 'status' messages because one 
       encapsulates the other */
    msg->status = New_status_struct();
    parseStatus(doc, ns, cur, msg->status);
  }
  else if( !xmlStrcmp(cur->name, (const xmlChar *) "IOType_defs") ){

    /* Record the message type */
    msg->msg_type = Get_message_type((const char *)(cur->name));

#if DEBUG
    fprintf(stderr, "Calling parseIOTypeDef...\n");
#endif
    msg->io_def = New_io_def_struct();
    parseIOTypeDef(doc, ns, cur, msg->io_def);
  }

  /* Print out what we've got */

#if DEBUG
  fprintf(stderr, "Calling Print_msg...\n");
  Print_msg(msg);
#endif

  /* Clean up everything else before quitting. */

  xmlCleanupParser();

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

      fprintf(stderr, "Calling parseParam...\n");
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

      fprintf(stderr, "Calling parseCmd...\n");
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

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Param") ){

      if( !ctrl->first_param ){

	ctrl->first_param = New_param_struct();
	ctrl->param = ctrl->first_param;
      }
      else{
        ctrl->param->next = New_param_struct();
        ctrl->param = ctrl->param->next;
      }

      fprintf(stderr, "Calling parseParam...\n");
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

#if DEBUG
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

#if DEBUG
      fprintf(stderr, "Calling parseIOType...\n");
#endif
      parseIOType(doc, ns, cur, io_def->io);
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

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Handle") && (cur->ns == ns)) {

      io->handle = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Label") && (cur->ns == ns)){

      io->label = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Direction") && (cur->ns == ns)){

      io->direction = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Support_auto_io") && (cur->ns == ns)){

      io->support_auto = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *) "Freq_handle") && (cur->ns == ns)){

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

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Command") && (cur->ns == ns)) {

      if( !supp_cmd->first_cmd ){

	supp_cmd->first_cmd = New_cmd_struct();
	supp_cmd->cmd = supp_cmd->first_cmd;
      }
      else{
	supp_cmd->cmd->next = New_cmd_struct();
	supp_cmd->cmd = supp_cmd->cmd->next;
      }

      parseCmd(doc, ns, cur, supp_cmd->cmd);
    }

    cur = cur->next;
  }

  return REG_SUCCESS;
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

    if( !xmlStrcmp(cur->name, (const xmlChar *) "Handle") && (cur->ns == ns)) {

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

struct msg_struct *New_msg_struct()
{

  struct msg_struct *msg;

  msg = (struct msg_struct *)malloc(sizeof(struct msg_struct));

  if(msg){

    msg->msg_type = MSG_NOTSET;
    msg->status   = NULL;
    msg->control  = NULL;
    msg->supp_cmd = NULL;
    msg->io_def   = NULL;
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
    io->support_auto = NULL;
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
    param->steerable = NULL;
    param->type   = NULL;
    param->is_internal = NULL;
    param->next   = NULL;
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
    cmd->first_param = NULL;
    cmd->param       = NULL;
    cmd->next        = NULL;
  }

  return cmd;
}

/*-----------------------------------------------------------------*/

void Delete_msg_struct(struct msg_struct *msg)
{

  if (!msg) return;

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

  free(msg);
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

    dum_ptr = cmd->next;

    if(cmd->first_param){
      Delete_param_struct(cmd->first_param);
      cmd->first_param = NULL;
      cmd->param       = NULL;
    }

    free(cmd);
    cmd = dum_ptr;
  }
}

/*-----------------------------------------------------------------*/

void Delete_io_def_struct(struct io_def_struct *io_def)
{
  if (!io_def) return;

  if(io_def->first_io){

    Delete_io_struct(io_def->first_io);
    io_def->first_io = NULL;
    io_def->io       = NULL;
  }

  free (io_def);
}

/*-----------------------------------------------------------------*/

void Delete_io_struct(struct io_struct *io)
{
  struct io_struct *dum_ptr;

  while(io){

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

    dum_ptr = param->next;
    free(param);
    param = dum_ptr;
  }
}

/*-----------------------------------------------------------------*/

void Print_msg(struct msg_struct *msg)
{
  if (!msg) {

    fprintf(stderr, "Print_msg: ptr to msg struct is null\n");
    return;
  }

  if(msg->status){

    fprintf(stderr, "Calling Print_status_struct...\n");
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
    if(ptr->id) fprintf(stderr, "   ID = %s\n", (char *)(ptr->id));
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

    if(ptr->label)fprintf(stderr, "Label = %s\n", 
			  (char *)(ptr->label));
    if(ptr->handle)fprintf(stderr, "Handle = %s\n", 
			   (char *)(ptr->handle));
    if(ptr->direction)fprintf(stderr, "Dirn = %s\n", 
			      (char *)(ptr->direction));
    if(ptr->support_auto)fprintf(stderr, "support_auto = %s\n", 
				(char *)(ptr->support_auto));
    if(ptr->freq_handle)fprintf(stderr, "Freq_handle = %s\n",
			       (char *)(ptr->freq_handle));

    ptr = ptr->next;
  }
  
}
