/*---------------------------------------------------------------------------
    This file contains a very simple example of a steering 
    application.

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
    23.7.2002       0.1                                         A Porter

---------------------------------------------------------------------------*/

#include "ReG_Steer_Steerside.h"

#define DEBUG 0

static int Edit_parameter(int sim_handle);

/*-------------------------------------------------------------------------*/

int main(){

  int   sim_handle;
  REG_MsgType msg_type;
  int   status;
  int   done;
  int   i;
  int   app_seqnum;
  int   num_cmds;
  int   commands[REG_MAX_NUM_STR_CMDS];
  char  user_char[2];

  int    num_params;
  int    param_handles[REG_INITIAL_NUM_PARAMS];
  char  *param_vals[REG_INITIAL_NUM_PARAMS];
  char  *param_labels[REG_INITIAL_NUM_PARAMS];
  int    num_types;
  int    io_handles[REG_INITIAL_NUM_IOTYPES];
  char  *io_labels[REG_INITIAL_NUM_IOTYPES];
  
  /* Initialise arrays for querying param values */

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    param_vals[i]   = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));
    param_labels[i] = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));

    if(param_vals[i]==NULL || param_labels[i]==NULL){
      printf("Error allocating memory for params - quitting\n");
      return 1;
    }
  }

  /* Initialise arrays for querying iotypes */
  
  for(i=0; i<REG_INITIAL_NUM_IOTYPES; i++){

    io_labels[i] = (char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));
  }

  /* Initialise tables etc. */

  if( (status = Steerer_initialize()) != REG_SUCCESS){

    printf("Steerer_initialize failed\n");
    return 1;
  }

  /* Attempt to attach to (just one) simulation */

  status = REG_FAILURE;

  while(status != REG_SUCCESS){

    system("sleep 2");
    status = Sim_attach(1, &sim_handle);
  }

  printf("Attached to sim, sim_handle = %d\n", sim_handle);

  done = FALSE;

  /* Enter main loop - wait on user commands */

  while(!done){

    /* Display the monitored parameters */

    if(Get_param_number(sim_handle, FALSE, &num_params) != REG_FAILURE){

      printf("Have %d monitored parameters:\n", num_params);

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
			  param_vals) != REG_FAILURE){

	for(i=0; i<num_params; i++){

	  printf("Handle: %d, Label: %s, Value: %s\n", param_handles[i], 
		 param_labels[i], param_vals[i]);
	}
      }
    }

    /* Wait for a command from the user */

    printf("\nCommand: ");

    while(TRUE){
      scanf("%c", user_char);
      if(user_char[0] != '\n' && user_char[0] != ' ')break;
    }

    switch(user_char[0]){

    case 'c':
      printf("Sending Consume command\n");
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
		    io_labels);

	/* Another cludge - should ask user which IOtype they want to
	   play with */
	for(i=0; i<num_types; i++){
	  commands[i] = io_handles[i];
	}
	Emit_control(sim_handle,
		     num_types,
		     commands);
      }
      break;

    case 'd':
      Get_supp_cmd_number(sim_handle,
			  &num_cmds);

      printf("We have %d supported commands:\n", num_cmds);

      /* Cludge rather than malloc and realloc */
      if(num_cmds > REG_MAX_NUM_STR_CMDS) num_cmds = REG_MAX_NUM_STR_CMDS;

      if( Get_supp_cmds(sim_handle,
		        num_cmds,
		        commands) != REG_FAILURE){

	for(i=0; i<num_cmds; i++){

	  printf("Supported command %d = %d\n", i, commands[i]);
	}
      }
      break;

    case 'e':
      if( Edit_parameter(sim_handle) == REG_SUCCESS){

	Emit_control(sim_handle,
		     0,
		     NULL);
      }
      else{
	printf("Failed to edit param values :(\n");
      }
      break;

    case 's':
      printf("Sending stop signal...\n");
      commands[0] = REG_STR_STOP;
      Emit_control(sim_handle,
		   1,
		   commands);

      Delete_sim_table_entry(&sim_handle);
      break;		   

    case 'p':
      printf("Pausing application...\n");
      commands[0] = REG_STR_PAUSE;
      Emit_control(sim_handle,
		   1,
		   commands);
      break;

    case 'q':
      done = TRUE;
      break;

    case 'r':
      printf("Resuming application...\n");
      commands[0] = REG_STR_RESUME;
      Emit_control(sim_handle,
		   1,
		   commands);
      break;

    case 'g':
      Get_next_message(&sim_handle,
		       &msg_type);

      switch(msg_type){

      case SUPP_CMDS:
	/* Supported commands should only be output once (as part
	   of handshaking process - read in Sim_attach) */
#if DEBUG
	printf("ERROR: Got supported cmds message\n");
#endif
	break;

      case MSG_NOTSET:
#if DEBUG
	printf("No messages to retrieve\n");
#endif
	break;

      case IO_DEFS:
#if DEBUG
	printf("Got IOdefs message\n");
#endif
	if(Consume_IOType_defs(sim_handle) != REG_SUCCESS){

	  printf("Consume_IOType_defs failed\n");
	}
	break;

      case PARAM_DEFS:
#if DEBUG
	printf("Got param defs message\n");
#endif
	if(Consume_param_defs(sim_handle) != REG_SUCCESS){

	  printf("Consume_param_defs failed\n");
	}
#if DEBUG
	Dump_sim_table();
#endif
	break;

      case STATUS:
#if DEBUG
	printf("Got status message\n");
#endif
	status = Consume_status(sim_handle,
				&app_seqnum,
				&num_cmds,
				commands);
	if(status == REG_FAILURE){
	  printf("Consume_status failed\n");
	  done = TRUE;
	}
	else{
#if DEBUG
	  Dump_sim_table();
#endif
	  /* Parse commands */
	  for(i=0; i<num_cmds; i++){

#if DEBUG
	    printf("Cmd %d = %d\n", i, commands[i]);
#endif
	    switch(commands[i]){

	    case REG_STR_DETACH:
#if DEBUG
	      printf("App has signalled that it has finished\n");
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
	  printf("Application SeqNum = %d\n", app_seqnum);
#endif
	}
	break;

      case CONTROL:
	printf("Got control message\n");
	break;

      default:
	printf("Unrecognised msg returned by Get_next_message\n");
	break;
      }
      break;

    default:
      break;
    }
  }

  if( (status = Sim_detach(&sim_handle)) != REG_SUCCESS){

    printf("Sim_detach failed...\n");
  }
  else{

    printf("Detached from sim, sim_handle = %d\n", sim_handle);
  }

  status = Steerer_finalize();

  if(status != REG_SUCCESS){

    printf("Steerer_finalize failed\n");
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
      printf("Error allocating memory for params - quitting\n");
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
    			param_vals) != REG_FAILURE){
    
      printf("Which of the following do you wish to edit?\n\n");
    
      for(i=0; i<num_params; i++){
    	printf("%d: %s = %s\n", i, param_labels[i], param_vals[i]);
      }

      printf("Enter choice or 'c' to cancel: ");

      while(TRUE){
	scanf("%s", user_str);
	if(user_str[0] != '\n' && user_str[0] != ' ')break;
      }

      printf("user_str[0] = %c\n", user_str[0]);

      if( !strchr(user_str, "c") ){

	if(sscanf(user_str, "%d", &input) == 1){

	  if(input >= 0 && input < num_params){

	    printf("Editing parameter %d, current value = %s...\n", 
		   input, param_vals[input]);

	    printf("New value: ");

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
	    printf("That is not a valid selection.\n");
	  }
	}
	else{
	  printf("Failed to get integer from input string\n");
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
