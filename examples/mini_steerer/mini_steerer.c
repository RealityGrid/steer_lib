/*---------------------------------------------------------------------------
    This file contains a very simple example of a steering 
    application.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the copyright holder, nor the
    University of Manchester offer any warranties or representations,
    nor do they accept any liabilities with respect to this software.

    This software must not be used for commercial gain without the
    written permission of the authors.
    
    This software must not be redistributed without the written
    permission of the authors.

    Permission is granted to modify this software, provided any
    modifications are made freely available to the original authors.
 
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL
    
    WWW:    http://www.sve.man.ac.uk  
    email:  sve@man.ac.uk
    Tel:    +44 161 275 6095
    Fax:    +44 161 275 6800    

    Initial version by:  A Porter, 23.7.2002

---------------------------------------------------------------------------*/

#include "ReG_Steer_Steerside.h"
#include <unistd.h>

#ifndef DEBUG
#define DEBUG 0
#endif

static int Edit_parameter(int sim_handle);

/*-------------------------------------------------------------------------*/

int main(){

  int    sim_handle;
  REG_MsgType msg_type;
  int    status;
  int    done;
  int    i;
  int    app_seqnum;
  int    num_cmds;
  int    commands[REG_MAX_NUM_STR_CMDS];
  char   user_char[2];
  char   user_str[REG_MAX_STRING_LENGTH];

  int    num_params;
  int    param_handles[REG_INITIAL_NUM_PARAMS];
  int    param_types[REG_INITIAL_NUM_PARAMS];
  char  *param_vals[REG_INITIAL_NUM_PARAMS];
  char  *param_labels[REG_INITIAL_NUM_PARAMS];
  int    num_types;

  int    io_handles[REG_INITIAL_NUM_IOTYPES];
  char  *io_labels[REG_INITIAL_NUM_IOTYPES];
  int    io_types[REG_INITIAL_NUM_IOTYPES];
  int    io_freqs[REG_INITIAL_NUM_IOTYPES];
  int    new_freq;

  int    nsims;
  char  *char_ptr;
  char  *sim_name[REG_MAX_NUM_STEERED_SIM];
  char  *sim_gsh[REG_MAX_NUM_STEERED_SIM];

  /* Initialise arrays for querying param values */

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    param_vals[i]   = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));
    param_labels[i] = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));

    if(param_vals[i]==NULL || param_labels[i]==NULL){
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

  char_ptr = (char *)malloc(REG_MAX_NUM_STEERED_SIM*REG_MAX_STRING_LENGTH*
			  sizeof(char));

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

  if( Get_sim_list(&nsims, sim_name, sim_gsh) == REG_SUCCESS){

    fprintf(stderr, "Steerable applications available:\n");
    for(i=0; i<nsims; i++){

      fprintf(stderr, "    id = %s, gsh = %s\n", sim_name[i], sim_gsh[i]);
    }
  }

  /* Attempt to attach to (just one) simulation */

  status = REG_FAILURE;

  while(status != REG_SUCCESS){

    sleep(2);

    /* If we got one from the framework then attempt to talk to that,
       otherwise default to old behaviour */
    if(nsims > 0){
      status = Sim_attach(sim_gsh[0], &sim_handle);
    }
    else{
      status = Sim_attach("DEFAULT", &sim_handle);
    }
  }

  fprintf(stderr, "Attached to sim, sim_handle = %d\n", sim_handle);

  done = FALSE;

  /* Enter main loop - wait on user commands */

  while(!done){

    /* Display the monitored parameters */

    if(Get_param_number(sim_handle, FALSE, &num_params) != REG_FAILURE){

      fprintf(stderr, "Have %d monitored parameters:\n", num_params);

      /* This is simple cludge - should really realloc to make arrays
	 big enough */
      if(num_params > REG_INITIAL_NUM_PARAMS){
	num_params = REG_INITIAL_NUM_PARAMS;
      }
      
      if(Get_param_values(sim_handle, 
			  FALSE,
			  num_params,
			  param_handles,
			  param_labels,
			  param_vals,
			  param_types) != REG_FAILURE){

	for(i=0; i<num_params; i++){

	  fprintf(stderr, "Handle: %d, Label: %s, Value: %s, Type: %d\n", 
		  param_handles[i], 
		  param_labels[i], param_vals[i], param_types[i]);
	}
      }
    }

    /* Display the steerable parameters */

    if(Get_param_number(sim_handle, TRUE, &num_params) != REG_FAILURE){

      fprintf(stderr, "Have %d steerable parameters:\n", num_params);

      /* This is simple cludge - should really realloc to make arrays
	 big enough */
      if(num_params > REG_INITIAL_NUM_PARAMS){
	num_params = REG_INITIAL_NUM_PARAMS;
      }
      
      if(Get_param_values(sim_handle, 
			  TRUE,
			  num_params,
			  param_handles,
			  param_labels,
			  param_vals,
			  param_types) != REG_FAILURE){

	for(i=0; i<num_params; i++){

	  fprintf(stderr, "Handle: %d, Label: %s, Value: %s, Type: %d\n", 
		  param_handles[i], 
		  param_labels[i], param_vals[i], param_types[i]);
	}
      }
    }

    /* Wait for a command from the user */

    fprintf(stderr, "\nCommand: ");

    while(TRUE){
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

	  while(TRUE){
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
      fprintf(stderr, "  s - send Stop signal to application\n");
      fprintf(stderr, "  p - send Pause signal to application\n");
      fprintf(stderr, "  q - Quit steerer - detaches from application\n");
      fprintf(stderr, "  r - send Resume signal to application\n");
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

    case 'p':
      /* Pause the application */
      fprintf(stderr, "Pausing application...\n");
      commands[0] = REG_STR_PAUSE;
      Emit_control(sim_handle,
		   1,
		   commands,
		   NULL);
      break;

    case 'q':
      /* Quit command */
      done = TRUE;
      break;

    case 'r':
      /* Resume a paused application */
      fprintf(stderr, "Resuming application...\n");
      commands[0] = REG_STR_RESUME;
      Emit_control(sim_handle,
		   1,
		   commands,
		   NULL);
      break;

    case 's':
      fprintf(stderr, "Sending stop signal...\n");
      commands[0] = REG_STR_STOP;
      Emit_control(sim_handle,
		   1,
		   commands,
		   NULL);

      Delete_sim_table_entry(&sim_handle);
      break;		   

    case 'g':
      Get_next_message(&sim_handle,
		       &msg_type);

      switch(msg_type){

      case SUPP_CMDS:
	/* Supported commands should only be output once (as part
	   of handshaking process - read in Sim_attach) */
#if DEBUG
	fprintf(stderr, "ERROR: Got supported cmds message\n");
#endif
	break;

      case MSG_NOTSET:
#if DEBUG
	fprintf(stderr, "No messages to retrieve\n");
#endif
	break;

      case IO_DEFS:
#if DEBUG
	fprintf(stderr, "Got IOdefs message\n");
#endif
	if(Consume_IOType_defs(sim_handle) != REG_SUCCESS){

	  fprintf(stderr, "Consume_IOType_defs failed\n");
	}
	break;

      case CHK_DEFS:
#if DEBUG
	fprintf(stderr, "Got Chkdefs message\n");
#endif
	if(Consume_ChkType_defs(sim_handle) != REG_SUCCESS){

	  fprintf(stderr, "Consume_ChkType_defs failed\n");
	}
	break;

      case PARAM_DEFS:
#if DEBUG
	fprintf(stderr, "Got param defs message\n");
#endif
	if(Consume_param_defs(sim_handle) != REG_SUCCESS){

	  fprintf(stderr, "Consume_param_defs failed\n");
	}
#if DEBUG
	Dump_sim_table();
#endif
	break;

      case STATUS:
#if DEBUG
	fprintf(stderr, "Got status message\n");
#endif
	status = Consume_status(sim_handle,
				&app_seqnum,
				&num_cmds,
				commands);
	if(status == REG_FAILURE){
	  fprintf(stderr, "Consume_status failed\n");
	  done = TRUE;
	}
	else{
#if DEBUG
	  Dump_sim_table();
#endif
	  /* Parse commands */
	  for(i=0; i<num_cmds; i++){

#if DEBUG
	    fprintf(stderr, "Cmd %d = %d\n", i, commands[i]);
#endif
	    switch(commands[i]){

	    case REG_STR_DETACH:
#if DEBUG
	      fprintf(stderr, "App has signalled that it has finished\n");
#endif
	      Delete_sim_table_entry(&sim_handle);
	      done = TRUE;
	      break;

	    default:
	      break;
	    }

	    if (done) break;
	  }

#if DEBUG
	  fprintf(stderr, "Application SeqNum = %d\n", app_seqnum);
#endif
	}
	break;

      case CONTROL:
	fprintf(stderr, "Got control message\n");
	break;

      default:
	fprintf(stderr, "Unrecognised msg returned by Get_next_message\n");
	break;
      }
      break;

    default:
      break;
    }
  }

  if( (status = Sim_detach(&sim_handle)) != REG_SUCCESS){

    fprintf(stderr, "Sim_detach failed...\n");
  }
  else{

    fprintf(stderr, "Detached from sim, sim_handle = %d\n", sim_handle);
  }

  status = Steerer_finalize();

  if(status != REG_SUCCESS){

    fprintf(stderr, "Steerer_finalize failed\n");
    return 1;
  }

  /* Clean up */

  /* Stuff for parameters */

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    if (param_vals[i]) free(param_vals[i]);
    if (param_labels[i]) free(param_labels[i]);
  }

  /* Stuff for IO definitions */

  for(i=0; i<REG_INITIAL_NUM_IOTYPES; i++){

    if (io_labels[i]) free(io_labels[i]);
  }

  return 0;
}

/*--------------------------------------------------------------------*/

int Edit_parameter(int sim_handle)
{
  int    num_params;
  int    param_handles[REG_INITIAL_NUM_PARAMS];
  int    param_types[REG_INITIAL_NUM_PARAMS];
  char  *param_vals[REG_INITIAL_NUM_PARAMS];
  char  *param_labels[REG_INITIAL_NUM_PARAMS];
  int    i;
  int    input;
  char   user_str[REG_MAX_STRING_LENGTH];
  int    return_status = REG_SUCCESS;

  /* Initialise arrays for querying param values */

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    param_vals[i]   = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));
    param_labels[i] = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));

    if(param_vals[i]==NULL || param_labels[i]==NULL){
      fprintf(stderr, "Error allocating memory for params - quitting\n");
      return REG_FAILURE;
    }
  }

  /* Find out what parameters there are to edit */
  
  if(Get_param_number(sim_handle, TRUE, &num_params) != REG_FAILURE){

    if(num_params > REG_INITIAL_NUM_PARAMS){
      num_params = REG_INITIAL_NUM_PARAMS;
    }
    
    if(Get_param_values(sim_handle, 
    			TRUE,
    			num_params,
    			param_handles,
    			param_labels,
    			param_vals,
			param_types) != REG_FAILURE){
    
      fprintf(stderr, "Which of the following do you wish to edit?\n\n");
    
      for(i=0; i<num_params; i++){
    	fprintf(stderr, "%d: %s = %s\n", i, param_labels[i], param_vals[i]);
      }

      fprintf(stderr, "Enter choice or 'c' to cancel: ");

      while(TRUE){
	scanf("%s", user_str);
	if(user_str[0] != '\n' && user_str[0] != ' ')break;
      }

      fprintf(stderr, "user_str[0] = %c\n", user_str[0]);

      if( !strchr(user_str, 'c') ){

	if(sscanf(user_str, "%d", &input) == 1){

	  if(input >= 0 && input < num_params){

	    fprintf(stderr, "Editing parameter %d, current value = %s...\n", 
		    input, param_vals[input]);

	    fprintf(stderr, "New value: ");

	    while(TRUE){
	      scanf("%s", user_str);
	      if(user_str[0] != '\n' && user_str[0] != ' ')break;
	    }

	    strcpy(param_vals[0], user_str);
	    return_status = Set_param_values(sim_handle,
					     1,
					     &(param_handles[input]),
					     param_vals);
	  }
	  else{
	    fprintf(stderr, "That is not a valid selection.\n");
	  }
	}
	else{
	  fprintf(stderr, "Failed to get integer from input string\n");
	}
      }
    }
  }
  else{

    return_status = REG_FAILURE;
  }
 
  /* Clean up */

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    if (param_vals[i]) free(param_vals[i]);
    if (param_labels[i]) free(param_labels[i]);
  }

  return return_status;
}
