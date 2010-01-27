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
    @file ReG_Steer_Steering_Transport_Files.c
    @brief Source file for file-based steering transport.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Steering_Transport_API.h"
#include "ReG_Steer_Steering_Transport_Files.h"
#include "ReG_Steer_Files_Common.h"
#include "ReG_Steer_Appside.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"

/** @internal
   The table holding details of our communication channel with the
   steering client - defined in ReG_Steer_Appside.c */
extern Steerer_connection_table_type Steerer_connection;

/** Log of values of parameters for which logging has
   been requested */
extern Chk_log_type Param_log;

/** Basic library config - declared in ReG_Steer_Common */
extern Steer_lib_config_type Steer_lib_config;

/*----------------- Appside methods ---------------------*/

int Detach_from_steerer_impl() {

  int   nbytes;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Remove lock file that indicates app is being steered */
  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",
		    Steer_lib_config.scratch_dir,
		    STR_CONNECTED_FILENAME);

  if(nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    fprintf(stderr, "STEER: Detach_from_steerer: name of lock-file exceeds %d"
	    " characters - increase REG_MAX_STRING_LENGTH\n",
	    REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  remove(filename);

  /* Remove any files that steerer has produced that we won't
     now be consuming */

  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",
		    Steer_lib_config.scratch_dir,
		    STR_TO_APP_FILENAME);

  if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    fprintf(stderr, "STEER: Detach_from_steerer: name of steerer ctrl files "
	    "exceeds %d characters - increase REG_MAX_STRING_LENGTH\n",
	    REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  remove_files(filename);

  /* send out all the param logs */
  Param_log.send_all         = REG_TRUE;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Steerer_connected_impl() {
  char   filename[REG_MAX_STRING_LENGTH];
  FILE  *fp;
  int    nbytes;

  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",
		    Steer_lib_config.scratch_dir,
		    STR_CONNECTED_FILENAME);

  if(nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)) {

    fprintf(stderr, "STEER: Steerer_connected: full path name of lockfile "
	    "indicating steerrer connected exceeds %d chars - increase "
	    "REG_MAX_STRING_LENGTH\n", REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  if((fp = fopen(filename, "r"))) {
      fclose(fp);
      return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------*/

int Send_status_msg_impl(char *buf) {
  FILE *fp;
  char  filename[REG_MAX_STRING_LENGTH];

  generate_status_filename(filename);

  if((fp = fopen(filename, "w")) == NULL) {

    fprintf(stderr, "STEER: Send_status_msg: failed to open file <%s>\n",
	    filename);
    return REG_FAILURE;
  }

  fprintf(fp, "%s", buf);
  fclose(fp);

  create_lock_file(filename);

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

struct msg_struct *Get_control_msg_impl() {
  struct msg_struct   *msg = NULL;
  FILE                *fp;
  char                 filename[REG_MAX_STRING_LENGTH];
  int                  nbytes;

  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",
		    Steer_lib_config.scratch_dir,
		    STR_TO_APP_FILENAME);

  if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    fprintf(stderr, "STEER: Get_control_msg: length of ctrl msg filename "
	    "exceeds %d chars - increase REG_MAX_STRING_LENGTH\n",
	    REG_MAX_STRING_LENGTH);
  }

  if((fp = open_next_file(filename)) != NULL) {

    fclose(fp);

    msg = New_msg_struct();

    /* Pass NULL down here as this is app-side and we have no ptr to
       a Sim_entry struct */
    if(Parse_xml_file(filename, msg, NULL) != REG_SUCCESS){

      fprintf(stderr, "STEER: Get_control_msg: failed to parse <%s>\n", filename);
      Delete_msg_struct(&msg);
    }

    /* Delete the file once we've read it */
    if(delete_file(filename) != REG_SUCCESS) {

      fprintf(stderr, "STEER: Get_control_msg: failed to delete %s\n",filename);
    }
  }

  return msg;
}

/*-------------------------------------------------------*/

int Initialize_steering_connection_impl(int  NumSupportedCmds,
					int *SupportedCmds) {
  FILE *fp;
  char  buf[REG_MAX_MSG_SIZE];
  char  filename[REG_MAX_STRING_LENGTH];

  strncpy(Steer_lib_config.Steering_transport_string, "Files", 6);

  /* Clean up any old files... */

  /* ...file indicating a steerer is connected (which it can't be since we've
     only just begun) */
  sprintf(filename, "%s%s", Steer_lib_config.scratch_dir,
	  STR_CONNECTED_FILENAME);
  fp = fopen(filename, "w");
  if(fp != NULL){

    fclose(fp);
    if(remove(filename)){

      fprintf(stderr, "STEER: Initialize_steering_connection: failed to "
	      "remove %s\n",filename);
    }
#ifdef REG_DEBUG
    else{
      fprintf(stderr, "STEER: Initialize_steering_connection: removed "
	      "%s\n", filename);
    }
#endif
  }

  /* ...files containing messages from a steerer */
  sprintf(filename, "%s%s", Steer_lib_config.scratch_dir,
	  STR_TO_APP_FILENAME);

  remove_files(filename);

  /* Signal that component is available to be steered */

  sprintf(filename, "%s%s", Steer_lib_config.scratch_dir,
	                    APP_STEERABLE_FILENAME);
  fp = fopen(filename,"w");

  if(fp == NULL){

    fprintf(stderr, "STEER: Initialize_steering_connection: failed to open %s\n",
	    filename);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Initialize_steering_connection: writing file: %s\n",
	  filename);
#endif

  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, buf, REG_MAX_MSG_SIZE);

  fprintf(fp, "%s", buf);
  fclose(fp);

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Finalize_steering_connection_impl() {
  char sys_command[REG_MAX_STRING_LENGTH];

#ifdef REG_DEBUG
  int  max, max1;

  max = strlen(APP_STEERABLE_FILENAME);
  max1 = strlen(STR_CONNECTED_FILENAME);

  if(max1 > max) max=max1;

  max += strlen(Steer_lib_config.scratch_dir);
  if(max > REG_MAX_STRING_LENGTH ){

    fprintf(stderr, "STEER: Finalize_steering_connection_file: WARNING: truncating "
	    "filename\n");
  }
#endif

  /* Delete the lock file that indicates we are steerable */
  sprintf(sys_command, "%s%s", Steer_lib_config.scratch_dir,
	  APP_STEERABLE_FILENAME);
  if(remove(sys_command)){

    fprintf(stderr, "STEER: Finalize_steering_connection_file: failed to remove "
	    "%s\n", sys_command);
  }

  /* Delete the lock file that indicates we are being steered */
  sprintf(sys_command, "%s%s", Steer_lib_config.scratch_dir,
	  STR_CONNECTED_FILENAME);
  if(remove(sys_command)){
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Finalize_steering_connection_file: failed to remove "
	    "%s\n", sys_command);
#endif
  }

  /* Delete any files we'd have consumed if we'd lived longer */
  sprintf(sys_command, "%s%s", Steer_lib_config.scratch_dir,
	  STR_TO_APP_FILENAME);
  remove_files(sys_command);

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Get_data_io_address_impl(const int           dummy,
			     const int           direction,
			     char*               hostname,
			     unsigned short int* port,
			     char*               label) {
  char *pchar;
  int   len;

  /* Return port = 0 on failure */
  *port = 0;

  /* Get hostname and port from environment variables */

  pchar = getenv("REG_CONNECTOR_HOSTNAME");
  if (pchar) {
    len = strlen(pchar);
    if (len < REG_MAX_STRING_LENGTH) {
      sprintf(hostname, pchar);
    }
    else{
      fprintf(stderr, "STEER: Get_data_source_address_file: content of "
	      "REG_CONNECTOR_HOSTNAME exceeds max. string length of "
	      "%d chars\n", REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }
  }
  else{
    fprintf(stderr,
	    "STEER: Get_data_source_address_file: REG_CONNECTOR_HOSTNAME not set\n");
    return REG_FAILURE;
  }

  pchar = getenv("REG_CONNECTOR_PORT");
  if (pchar) {
    *port =  (unsigned short int)atoi(pchar);
  }
  else{
    fprintf(stderr,
	    "STEER: Get_data_source_address_file: REG_CONNECTOR_PORT not set\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Record_checkpoint_set_impl(int ChkType, char* ChkTag, char* Path) {
  return Record_Chkpt(ChkType, ChkTag);
}

/*------------------ Steerside methods ------------------*/

extern Sim_table_type Sim_table;

int Initialize_steerside_transport() {
  strncpy(Steer_lib_config.Steering_transport_string, "Files", 6);

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Finalize_steerside_transport() {
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Sim_attach_impl(int index, char *SimID) {
  char *pchar;
  char  file_root[REG_MAX_STRING_LENGTH];
  char  filename[REG_MAX_STRING_LENGTH];
  int   return_status;
  Sim_entry_type *sim;

  return_status = REG_FAILURE;
  sim = &(Sim_table.sim[index]);

  /* Unless SimID contains a string, get the directory to use for
     steering messages from the REG_STEER_DIRECTORY env. variable.  */
  if(strlen(SimID) != 0){
    pchar = SimID;

    /* Check that path ends in '/' - if not then add one */
    if( pchar[strlen(pchar)-1] != '/' ){

      sprintf(file_root, "%s/", pchar);
    }
    else{
      strcpy(file_root, pchar);
    }

    return_status = Directory_valid(file_root);
#ifdef REG_DEBUG
    if(return_status != REG_SUCCESS){
      fprintf(stderr, "STEER: Sim_attach: invalid dir for "
	      "steering messages: %s\n", file_root);
    }
#endif
  }

  /* If supplied ID didn't work then try using REG_STEER_DIRECTORY env.
     variable instead */
  if(return_status != REG_SUCCESS){

    if(!(pchar = getenv("REG_STEER_DIRECTORY"))){
      fprintf(stderr, "STEER: Sim_attach: failed to get scratch directory\n");
      return REG_FAILURE;
    }

    /* Check that path ends in '/' - if not then add one */
    if( pchar[strlen(pchar)-1] != '/' ){

      sprintf(file_root, "%s/", pchar);
    }
    else{
      strcpy(file_root, pchar);
    }

    return_status = Directory_valid(file_root);
#ifdef REG_DEBUG
    if(return_status != REG_SUCCESS){
      fprintf(stderr, "STEER: Sim_attach: invalid dir for "
	      "steering messages: %s\n", file_root);
    }
#endif
  }

  if(return_status != REG_SUCCESS){
    fprintf(stderr, "STEER: Sim_attach: failed to get scratch directory\n");
    return REG_FAILURE;
  }
#ifdef REG_DEBUG
  else{
    fprintf(stderr, "STEER: Sim_attach: using following dir for "
	    "steering messages: %s\n", file_root);
  }
#endif

  /* Delete any old communication files written by an app
     in this location */

  sprintf(filename, "%s%s", file_root,
	  APP_TO_STR_FILENAME);

  remove_files(filename);

  /* Save directory used for communication */
  strcpy(sim->file_root, file_root);

  /* Read the commands that the application supports */
  return_status = consume_supp_cmds(index);

  if(return_status == REG_SUCCESS){

    /* Create lock file to indicate that steerer has connected to sim */
    int fd;
    char lock_file[REG_MAX_STRING_LENGTH];

    sprintf(lock_file, "%s%s", sim->file_root, STR_CONNECTED_FILENAME);
    if((fd = creat(lock_file,
		   (S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH))) < 0) {
      return REG_FAILURE;
    }

    close(fd);
  }

  return return_status;
}

/*-------------------------------------------------------*/

int Sim_attach_security_impl(const int index,
			     const struct reg_security_info* sec) {
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

struct msg_struct *Get_status_msg_impl(int index, int ignore) {
  struct msg_struct *msg = NULL;
  char  filename[REG_MAX_STRING_LENGTH];
  FILE *fp;
  int   return_status;
  Sim_entry_type *sim;

  sim = &(Sim_table.sim[index]);

  sprintf(filename, "%s%s", sim->file_root, APP_TO_STR_FILENAME);

  if((fp = open_next_file(filename))) {

    fclose(fp);

    /* Parse it and store it in structure pointed to by msg */

    msg = New_msg_struct();

    return_status = Parse_xml_file(filename, msg, sim);

    if(return_status != REG_SUCCESS){

      Delete_msg_struct(&msg);
    }

    /* Consume the file now that we've read it */
    delete_file(filename);
  }

  return msg;
}

/*-------------------------------------------------------*/

int Send_control_msg_impl(int index, char* buf) {
  FILE *fp;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Write to a 'local' file */

  if(generate_control_filename(index, filename) != REG_SUCCESS) {
    fprintf(stderr, "STEER: Send_control_msg: failed to create filename\n");
    return REG_FAILURE;
  }

  if((fp = fopen(filename, "w")) == NULL) {
    fprintf(stderr, "STEER: Send_control_msg: failed to open file\n");
    return REG_FAILURE;
  }

  fprintf(fp, "%s", buf);
  fclose(fp);

  /* The application only attempts to read files for which it can find an
     associated lock file */
  return create_lock_file(filename);
}

/*-------------------------------------------------------*/

int Send_detach_msg_impl(int index) {
  int command[1];
  Sim_entry_type *sim;

  sim = &(Sim_table.sim[index]);
  command[0] = REG_STR_DETACH;

  return Emit_control(sim->handle, 1, command, NULL);
}

/*-------------------------------------------------------*/

int Finalize_connection_impl(int index) {
  char base_name[REG_MAX_STRING_LENGTH];

  /* Delete any files that the app's produced that we won't now be
     consuming */

  sprintf(base_name, "%s%s", Sim_table.sim[index].file_root,
	  APP_TO_STR_FILENAME);

  remove_files(base_name);

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Get_param_log_impl(int index, int handle) {
  int           command[1];
  static char** command_params = NULL;

  command[0] = REG_STR_EMIT_PARAM_LOG;

  /* Which parameter to get the log of */
  if(!command_params) command_params = Alloc_string_array(32, 1);
  sprintf(command_params[0], "%d", handle);

  return Emit_control(Sim_table.sim[index].handle,
		      1,
		      command,
		      command_params);
}

/*-------------------------------------------------------*/

int Get_registry_entries_impl(const char* registryEPR,
			      const struct reg_security_info* sec,
			      struct registry_contents* contents) {
  return REG_SUCCESS;
}

/*---------------- Internal methods ---------------------*/

int generate_status_filename(char* filename) {

  static int output_file_index = 0;

  /* Generate next filename in sequence for sending data to
     steerer & increment counter */

  sprintf(filename, "%s%s_%d", Steer_lib_config.scratch_dir,
	  APP_TO_STR_FILENAME, output_file_index++);

  /* Wrap counter if no. of distinct files exceeded */

  if(output_file_index == REG_MAX_NUM_FILES)
    output_file_index = 0;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int generate_control_filename(int index, char* filename) {
  static int output_file_index = 0;

  /* Generate next filename in sequence for sending data to
     steerer & increment counter */

  sprintf(filename, "%s%s_%d", Sim_table.sim[index].file_root,
	  STR_TO_APP_FILENAME, output_file_index++);

  if(output_file_index == REG_MAX_NUM_FILES)
    output_file_index = 0;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int consume_supp_cmds(int index) {
  FILE              *fp1;
  FILE              *fp2;
  char               filename[REG_MAX_STRING_LENGTH];
  struct msg_struct *msg;
  struct cmd_struct *cmd;
  int                return_status = REG_SUCCESS;
  Sim_entry_type *sim;

  sim = &(Sim_table.sim[index]);

  /* Check for absence of lock file indicating that sim is
     already being steered */

  sprintf(filename, "%s%s", sim->file_root, STR_CONNECTED_FILENAME);
  fp2 = fopen(filename, "r");

  /* Check for presence of lock file indicating sim is steerable */

  sprintf(filename, "%s%s", sim->file_root, APP_STEERABLE_FILENAME);
  fp1 = fopen(filename, "r");

  if(fp1 != NULL && fp2 == NULL){

    fclose(fp1);

    msg = New_msg_struct();

    return_status = Parse_xml_file(filename, msg, sim);

    if(return_status == REG_SUCCESS){

      cmd = msg->supp_cmd->first_cmd;

      while(cmd){

	sscanf((char *)(cmd->id), "%d",
	       &(sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id));

	/* ARPDBG - may need to add cmd parameters here too */

	Increment_cmd_registered(&(sim->Cmds_table));

	cmd = cmd->next;
      }
    }
    else{

      fprintf(stderr, "STEER: Consume_supp_cmds_file: error parsing <%s>\n",
	      filename);
    }

    Delete_msg_struct(&msg);
  }
  else{

    if(fp2) fclose(fp2);

    return REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------*/
