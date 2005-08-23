/*---------------------------------------------------------------------------
  This file contains a very simple example of a steering 
  application.

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

#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Browser.h"
#include <string.h>
#include <unistd.h>

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

static int Edit_parameter(int sim_handle);
static int Choose_parameter(int sim_handle, int steerable);

/*-------------------------------------------------------------------------*/

int main(int argc, char **argv){

  int    sim_handle;
  int    msg_type;
  int    status;
  int    done;
  int    i, j, k;
  int    app_seqnum;
  int    num_cmds;
  int    commands[REG_MAX_NUM_STR_CMDS];
  char   user_char[2];
  char   user_str[REG_MAX_STRING_LENGTH];
  char   chk_GSH[REG_MAX_STRING_LENGTH];

  int    num_params;
  char  *param_vals[REG_INITIAL_NUM_PARAMS];
  Param_details_struct param_details[REG_INITIAL_NUM_PARAMS];
  int    num_types;
  int    handle;

  int    io_handles[REG_INITIAL_NUM_IOTYPES];
  char  *io_labels[REG_INITIAL_NUM_IOTYPES];
  int    io_types[REG_INITIAL_NUM_IOTYPES];
  int    io_freqs[REG_INITIAL_NUM_IOTYPES];
  int    new_freq;

  int    nsims;
  char  *char_ptr;
  char  *pchar;
  char  *sim_name[REG_MAX_NUM_STEERED_SIM];
  char  *sim_gsh[REG_MAX_NUM_STEERED_SIM];

  int                num_entries;
  Output_log_struct  chk_entries[10];
  double            *log_ptr;

  /* Initialise arrays for querying param values */

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    param_vals[i]   = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));
    if(param_vals[i]==NULL ){
      fprintf(stderr, "Error allocating memory for params - quitting\n");
      return 1;
    }
  }

  /* Initialise arrays for querying iotypes */
  
  for(i=0; i<REG_INITIAL_NUM_IOTYPES; i++){

    io_labels[i] = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));
  }

  /* Initialise tables etc. */

  if( (status = Steerer_initialize()) != REG_SUCCESS){

    fprintf(stderr, "Steerer_initialize failed\n");
    return 1;
  }

  /* Get list of steerable simulations */

  pchar = (char *)malloc(2*REG_MAX_NUM_STEERED_SIM*REG_MAX_STRING_LENGTH*
			 sizeof(char));
  char_ptr = pchar;
  if(!char_ptr){
    fprintf(stderr, "malloc for application list failed\n");
    return 1;
  }

  for(i=0; i<REG_MAX_NUM_STEERED_SIM; i++){

    sim_name[i] = char_ptr;
    char_ptr += REG_MAX_STRING_LENGTH;
    sim_gsh[i]  = char_ptr;
    char_ptr += REG_MAX_STRING_LENGTH;
  }
  char_ptr = NULL;

  /* Take GSH/EPR off command line if supplied */
  if(argc == 2 && strstr(argv[1], "http://")){
    status = Sim_attach(argv[1], &sim_handle);
  }
  else{
    printf("Do (l)ocal or (r)emote attach: ");
    while(REG_TRUE){
      scanf("%c", user_char);
      if(user_char[0] != '\n' && user_char[0] != ' ')break;
    }

    if(user_char[0] == 'l' || user_char[0] == 'L'){

      status = Sim_attach("", &sim_handle);
    }
    else{

      Get_sim_list(&nsims, sim_name, sim_gsh);

      /* Attempt to attach to (just one) simulation - this blocks */

      status = REG_FAILURE;

      while(status != REG_SUCCESS){

	sleep(2);

	/* If we got one from the framework then attempt to talk to that,
	   otherwise default to old behaviour */
	if(nsims > 0){

	  printf("\n%d steerable applications available:\n", nsims);
	  for(i=0; i<nsims; i++){

	    printf("    %d: %s, gsh = %s\n", i, sim_name[i], sim_gsh[i]);
	  }

	  i = -1;
	  while(i<0 || i>(nsims-1)){
	    printf("\nWhich one to attach to (0 - %d): ", nsims-1);
	    while(REG_TRUE){
	      if(scanf("%d", &i) == 1)break;
	    }
	    printf("\n");
	  }

	  status = Sim_attach(sim_gsh[i], &sim_handle);
	}
	else{
	  status = Sim_attach("", &sim_handle);
	}

	if(status != REG_SUCCESS){
	  fprintf(stderr, "Attach failed :-(\n");
	}
      }
    }
  }

  /* Done with malloc'd memory */
  free(pchar);

  if(status != REG_SUCCESS){

    printf("Failed to attach to a simulation\n");
    return 1;
  }

  fprintf(stderr, "Attached to sim, sim_handle = %d\n", sim_handle);

  done = REG_FALSE;

  /* Enter main loop - wait on user commands */

  while(!done){

    /* Display the monitored parameters */

    if(Get_param_number(sim_handle, REG_FALSE, &num_params) != REG_FAILURE){

      fprintf(stderr, "Have %d monitored parameters:\n", num_params);

      /* This is simple cludge - should really realloc to make arrays
	 big enough */
      if(num_params > REG_INITIAL_NUM_PARAMS){
	num_params = REG_INITIAL_NUM_PARAMS;
      }
      
      if(Get_param_values(sim_handle, 
			  REG_FALSE,
			  num_params,
			  param_details) != REG_FAILURE){

	for(i=0; i<num_params; i++){

	  fprintf(stderr, "Handle: %d, Label: %s, Value: %s, Type: %d\n", 
		  param_details[i].handle, param_details[i].label, 
		  param_details[i].value, param_details[i].type);

	  if(param_details[i].type == REG_BIN){
	    fprintf(stderr, "        Have %d bytes of raw data at %p\n", 
		    atoi(param_details[i].max_val), 
		    param_details[i].raw_data);
	  }

	}
      }
    }

    /* Display the steerable parameters */

    if(Get_param_number(sim_handle, REG_TRUE, &num_params) != REG_FAILURE){

      fprintf(stderr, "\nHave %d steerable parameters:\n", num_params);

      /* This is simple cludge - should really realloc to make arrays
	 big enough */
      if(num_params > REG_INITIAL_NUM_PARAMS){
	num_params = REG_INITIAL_NUM_PARAMS;
      }
      
      if(Get_param_values(sim_handle, 
			  REG_TRUE,
			  num_params,
			  param_details) != REG_FAILURE){


	for(i=0; i<num_params; i++){

	  fprintf(stderr, "Handle: %d, Label: %s, Value: %s, Type: %d\n", 
		  param_details[i].handle, 
		  param_details[i].label, param_details[i].value, 
		  param_details[i].type);
	}
      }
    }

    /* Wait for a command from the user */

    fprintf(stderr, "\nCommand: ");

    while(REG_TRUE){
      scanf("%c", user_char);
      if(user_char[0] != '\n' && user_char[0] != ' ')break;
    }

    switch(user_char[0]){

    case 'c':
      fprintf(stderr, "Sending Consume command\n");
      Get_iotype_number(sim_handle,
			&num_types);

      if(num_types > 0){

	/* Should really realloc if insufficient memory but this is a simple
	   test code so don't bother here */
	if(num_types > REG_INITIAL_NUM_IOTYPES){
	  num_types = REG_INITIAL_NUM_IOTYPES;
	}

	Get_iotypes(sim_handle,
		    num_types,
		    io_handles,
		    io_labels,
		    io_types,
		    io_freqs);

	/* Another cludge - should ask user which IOtype they want to
	   play with */
	for(i=0; i<num_types; i++){
	  commands[i] = io_handles[i];

	  if(io_types[i] == REG_IO_OUT){

	    sprintf(param_vals[i], "OUT");
	  }
	  else{
            sprintf(param_vals[i], "IN");
	  }
	}
	Emit_control(sim_handle,
		     num_types,
		     commands,
		     param_vals);
      }
      break;

    case 'd':
      Get_supp_cmd_number(sim_handle,
			  &num_cmds);

      fprintf(stderr, "We have %d supported commands:\n", num_cmds);

      /* Cludge rather than malloc and realloc */
      if(num_cmds > REG_MAX_NUM_STR_CMDS) num_cmds = REG_MAX_NUM_STR_CMDS;

      if( Get_supp_cmds(sim_handle,
		        num_cmds,
		        commands) != REG_FAILURE){

	for(i=0; i<num_cmds; i++){

	  fprintf(stderr, "Supported command %d = %d\n", i, commands[i]);
	}
      }
      break;

    case 'e':

      if( Edit_parameter(sim_handle) == REG_SUCCESS){

	/* Emit_control automatically emits the values of any steerable
	   parameters that have been edited since it was last called */
	Emit_control(sim_handle,
		     0,
		     NULL,
		     NULL);
      }
      else{
	fprintf(stderr, "Failed to edit param values :(\n");
      }
      break;

    case 'f':
      /* Edit IO consume/emit frequency */
      Get_iotype_number(sim_handle,
			&num_types);

      if(num_types > 0){

	/* Should really realloc if insufficient memory but this is a simple
	   test code so don't bother here */
	if(num_types > REG_INITIAL_NUM_IOTYPES){
	  num_types = REG_INITIAL_NUM_IOTYPES;
	}

	Get_iotypes(sim_handle,
		    num_types,
		    io_handles,
		    io_labels,
		    io_types,
		    io_freqs);

	for(i=0; i<num_types; i++){

	  /* Just edit the first one we find... */
	  fprintf(stderr, 
	  	    "Freq. of iotype %s = %d\n", io_labels[i], io_freqs[i]);
	  fprintf(stderr, "Enter new value: ");

	  while(REG_TRUE){
	    scanf("%s", user_str);
	    if(user_str[0] != '\n' && user_str[0] != ' ')break;
	  }

	  sscanf(user_str, "%d", &new_freq);

	  fprintf(stderr, "\nSetting frequency to %d\n", new_freq);

	  Set_iotype_freq(sim_handle, 1, &(io_handles[i]), &new_freq);

	  Emit_control(sim_handle,
	  		 0,
	  		 NULL,
	  		 NULL);

	  break;
	}
      }
      break;

    case 'h':
      fprintf(stderr, "Possible commands are:\n");
      fprintf(stderr, "  c - send Consume command\n");
      fprintf(stderr, "  d - Display supported commands\n");
      fprintf(stderr, "  e - Edit steerable parameter\n");
      fprintf(stderr, "  f - edit emit/consume Frequency of IOtype\n");
      fprintf(stderr, "  g - Get next message from application\n");
      fprintf(stderr, "  h - display this help message\n");
      fprintf(stderr, "  l - display List of checkpoint types\n");
      fprintf(stderr, "  o - retrieve param hist. log from application\n");
      fprintf(stderr, "  p - send Pause signal to application\n");
      fprintf(stderr, "  q - Quit steerer - detaches from application\n");
      fprintf(stderr, "  r - send Resume signal to application\n");
      fprintf(stderr, "  R - send Restart signal to application\n");
      fprintf(stderr, "  s - send Stop signal to application\n");
      fprintf(stderr, "  v - view logged checkpoints\n");
      fprintf(stderr, "  V - view logged parameters\n");
      fprintf(stderr, "\n");
      break;

    case 'l':
      Get_chktype_number(sim_handle,
			 &num_types);

      if(num_types > 0){

	/* Should really realloc if insufficient memory but this is a simple
	   test code so don't bother here */
	if(num_types > REG_INITIAL_NUM_IOTYPES){
	  num_types = REG_INITIAL_NUM_IOTYPES;
	}

	Get_chktypes(sim_handle,
		     num_types,
		     io_handles,
		     io_labels,
		     io_types,
		     io_freqs);

	for(i=0; i<num_types; i++){

	  fprintf(stderr, "Chktype #%d:\n", i);
	  fprintf(stderr, "  frequency = %d\n", io_freqs[i]);
	  fprintf(stderr, "  label     = %s\n", io_labels[i]);
	  fprintf(stderr, "  direction = %d\n", io_types[i]);
	}
      }
      break;

    case 'o':
      /* Get the log of parameters back */
      handle = Choose_parameter(sim_handle, REG_FALSE);

      if(handle != REG_PARAM_HANDLE_NOTSET)
	Emit_retrieve_param_log_cmd(sim_handle, handle);
      break;

    case 'p':
      /* Pause the application */
      fprintf(stderr, "Pausing application...\n");
      Emit_pause_cmd(sim_handle);
      break;

    case 'q':
      /* Quit command */
      done = REG_TRUE;
      break;

    case 'r':
      /* Resume a paused application */
      fprintf(stderr, "Resuming application...\n");
      Emit_resume_cmd(sim_handle);
      break;

    case 'R':
      /* This only works if we're steering in a Grid Service 
	 framework - otherwise a GSH is meaningless */
      printf("Enter GSH of checkpoint to restart from: ");
      while(REG_TRUE){
	scanf("%s", chk_GSH);
	if(chk_GSH[0] != '\n' && chk_GSH[0] != ' ')break;
      }
      Emit_restart_cmd(sim_handle, chk_GSH);
      break;

    case 's':
      fprintf(stderr, "Sending stop signal...\n");
      Emit_stop_cmd(sim_handle);
      break;		   

    case 'g':
      Get_next_message(&sim_handle,
		       &msg_type);

      switch(msg_type){

      case SUPP_CMDS:
	/* Supported commands should only be output once (as part
	   of handshaking process - read in Sim_attach) */
#if REG_DEBUG
	fprintf(stderr, "ERROR: Got supported cmds message\n");
#endif
	break;

      case MSG_NOTSET:
#if REG_DEBUG
	fprintf(stderr, "No messages to retrieve\n");
#endif
	break;

      case IO_DEFS:
#if REG_DEBUG
	fprintf(stderr, "Got IOdefs message\n");
#endif
	if(Consume_IOType_defs(sim_handle) != REG_SUCCESS){

	  fprintf(stderr, "Consume_IOType_defs failed\n");
	}
	break;

      case CHK_DEFS:
#if REG_DEBUG
	fprintf(stderr, "Got Chkdefs message\n");
#endif
	if(Consume_ChkType_defs(sim_handle) != REG_SUCCESS){

	  fprintf(stderr, "Consume_ChkType_defs failed\n");
	}
	break;

      case PARAM_DEFS:
#if REG_DEBUG
	fprintf(stderr, "Got param defs message\n");
#endif
	if(Consume_param_defs(sim_handle) != REG_SUCCESS){

	  fprintf(stderr, "Consume_param_defs failed\n");
	}
#if REG_DEBUG
	Dump_sim_table();
#endif
	break;

      case STATUS:
#if REG_DEBUG
	fprintf(stderr, "Got status message\n");
#endif
	status = Consume_status(sim_handle,
				&app_seqnum,
				&num_cmds,
				commands);
	if(status == REG_FAILURE){
	  fprintf(stderr, "Consume_status failed\n");
	  done = REG_TRUE;
	}
	else{
#if REG_DEBUG
	  Dump_sim_table();
#endif
	  /* Parse commands */
	  for(i=0; i<num_cmds; i++){

#if REG_DEBUG
	    fprintf(stderr, "Cmd %d = %d\n", i, commands[i]);
#endif
	    switch(commands[i]){

	    case REG_STR_STOP:
	    case REG_STR_DETACH:
#if REG_DEBUG
	      fprintf(stderr, "App has signalled that it has finished\n");
#endif
	      Delete_sim_table_entry(&sim_handle);
	      done = REG_TRUE;
	      break;

	    default:
	      break;
	    }

	    if (done) break;
	  }

#if REG_DEBUG
	  fprintf(stderr, "Application SeqNum = %d\n", app_seqnum);
#endif
	}
	break;

      case CONTROL:
	fprintf(stderr, "Got control message\n");
	break;

      case STEER_LOG:
	fprintf(stderr, "Got log message\n");
	Consume_log(sim_handle);
	break;

      case MSG_ERROR:
	fprintf(stderr, "Error getting message - assume app finished\n");
	Delete_sim_table_entry(&sim_handle);
	done = REG_TRUE;
	break;

      default:
	fprintf(stderr, "Unrecognised msg returned by Get_next_message\n");
	break;
      }
      break;

    case 'v':
      Get_chktype_number(sim_handle,
			 &num_types);

      if(num_types > 0){

	if(num_types > REG_INITIAL_NUM_IOTYPES){
	  num_types = REG_INITIAL_NUM_IOTYPES;
	}
	/* This section only applicable to file-based checkpoint
	   logging.  In a Grid Service framework the user would 
	   browse the checkpoint tree to see what's available. */
	Get_chktypes(sim_handle,
		     num_types,
		     io_handles,
		     io_labels,
		     io_types,
		     io_freqs);

	for(i=0; i<num_types; i++){

	  if(io_types[i] == REG_IO_INOUT){

	    Get_chk_log_number(sim_handle,
			       io_handles[i],
			       &num_entries);

	    fprintf(stderr, "We have %d log entries for checkpoint "
		    "%s:\n", num_entries, io_labels[i]);

	    if(num_entries > 10) num_entries = 10;

	    Get_chk_log_entries_reverse(sim_handle,
					io_handles[i],
					num_entries,
					chk_entries);

	    for(j=0; j<num_entries; j++){
	      fprintf(stderr, "  Tag: %s\n", chk_entries[j].chk_tag);

	      for(k=0; k<chk_entries[j].num_param; k++){

		fprintf(stderr, "    Param <%s> = %s\n",
			chk_entries[j].param_labels[k],
			chk_entries[j].param_values[k]);
	      }
	    }
	  }
	}
      }
      break;

    case 'V':

      handle = Choose_parameter(sim_handle, REG_FALSE);
      if(handle == REG_PARAM_HANDLE_NOTSET)break;

      if(Get_param_log(sim_handle,	    /* ReG library */
		       handle,
		       &(log_ptr),
		       &i) == REG_SUCCESS){
	printf("Got %d param log entries for handle %d\n",
	       i, handle);

	for(j=0; j<i; j++){
	  printf("%f ", log_ptr[j]);
	}
	printf("\n\n");
      }

      break;

    default:
      fprintf(stderr, "Type h for list of commands...\n");
      break;
    }
  }

  if( (status = Sim_detach(&sim_handle)) != REG_SUCCESS){

    fprintf(stderr, "Sim_detach failed...\n");
  }
  else{

    fprintf(stderr, "Detached from sim, sim_handle = %d\n", sim_handle);
  }

  /* Detach (if not already detached) and clean up the steering lib */
  status = Steerer_finalize();

  if(status != REG_SUCCESS){

    fprintf(stderr, "Steerer_finalize failed\n");
    return 1;
  }

  /* Clean up locals */

  /* Stuff for parameters */

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    if (param_vals[i]) {
      free(param_vals[i]);
      param_vals[i] = NULL;
    }
  }

  /* Stuff for IO definitions */

  for(i=0; i<REG_INITIAL_NUM_IOTYPES; i++){

    if (io_labels[i]) free(io_labels[i]);
  }

  return 0;
}

/*--------------------------------------------------------------------*/

int Choose_parameter(int sim_handle, int steerable)
{
  int    num_params;
  Param_details_struct param_details[REG_INITIAL_NUM_PARAMS];
  int    i, input;
  char   user_str[REG_MAX_STRING_LENGTH];

  /* Find out what parameters there are available */
  
  if(Get_param_number(sim_handle, steerable, &num_params) != REG_FAILURE){

    if(num_params > REG_INITIAL_NUM_PARAMS){
      num_params = REG_INITIAL_NUM_PARAMS;
    }
    
    if(Get_param_values(sim_handle, 
    			steerable,
    			num_params,
			param_details) != REG_FAILURE){
    
      fprintf(stderr, "Which parameter to select?\n\n");
    
      for(i=0; i<num_params; i++){
    	fprintf(stderr, "%d: %s = %s (%s,%s)\n", i, param_details[i].label, 
		param_details[i].value, param_details[i].min_val, 
		param_details[i].max_val);
      }

      fprintf(stderr, "Enter choice or 'c' to cancel: ");

      while(REG_TRUE){
	scanf("%s", user_str);
	if(user_str[0] != '\n' && user_str[0] != ' ')break;
      }

      if( strchr(user_str, 'c') ){return REG_PARAM_HANDLE_NOTSET;}

      if(sscanf(user_str, "%d", &input) == 1){

	if(input < 0 || input >= num_params){
	  fprintf(stderr, "That is not a valid selection.\n");
	  return REG_PARAM_HANDLE_NOTSET;
	}

	return param_details[input].handle;
      }
    }
  }


  return REG_PARAM_HANDLE_NOTSET;
}

/*--------------------------------------------------------------------*/

int Edit_parameter(int sim_handle)
{
  int    num_params;
  char  *param_val[1];
  Param_details_struct param_details[REG_INITIAL_NUM_PARAMS];
  int    i, index;
  char   user_str[REG_MAX_STRING_LENGTH];
  int    return_status = REG_SUCCESS;
  int    handle;

  /* Choose a parameter to edit */
  handle = Choose_parameter(sim_handle, REG_TRUE);

  if(handle == REG_PARAM_HANDLE_NOTSET)return REG_FAILURE;

  param_val[0] = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));
  if (!param_val[0]) return REG_FAILURE;

  /* Get the details of this parameter */  
  if(Get_param_number(sim_handle, REG_TRUE, &num_params) != REG_FAILURE){

    if(num_params > REG_INITIAL_NUM_PARAMS){
      num_params = REG_INITIAL_NUM_PARAMS;
    }
    
    if(Get_param_values(sim_handle, 
    			REG_TRUE,
    			num_params,
			param_details) != REG_FAILURE){
    
      for(i=0; i<num_params; i++){

	if(param_details[i].handle == handle){
	  fprintf(stderr, "%d: %s = %s (%s,%s)\n", i, param_details[i].label, 
		  param_details[i].value, param_details[i].min_val, 
		  param_details[i].max_val);
	  index = i;
	  break;
	}
      }

      fprintf(stderr, "Editing parameter with handle %d, "
	      "current value = %s...\n", 
	      handle, param_details[index].value);

      fprintf(stderr, "New value: ");

      while(REG_TRUE){
	scanf("%s", user_str);
	if(user_str[0] != '\n' && user_str[0] != ' ')break;
      }

      strcpy(param_val[0], user_str);
      return_status = Set_param_values(sim_handle,
				       1,
				       &(param_details[index].handle),
				       param_val);
    }
  }
  else{

    return_status = REG_FAILURE;
  }

  if(param_val[0]){
    free(param_val[0]);
    param_val[0] = NULL;
  } 

  return return_status;
}
