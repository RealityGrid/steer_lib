/*---------------------------------------------------------------------------
    This file contains a very simple example of a steering-enabled 
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

#include "ReG_Steer_Appside.h"

int main(){

  /* For steering */

  int    input_iotype;
  char*  iotype_labels[REG_INITIAL_NUM_IOTYPES];
	 
  char*  param_labels[REG_INITIAL_NUM_PARAMS];
  void*  param_ptrs[REG_INITIAL_NUM_PARAMS];
  int    param_types[REG_INITIAL_NUM_PARAMS];
  int    param_strbl[REG_INITIAL_NUM_PARAMS];
	 
  int    status;
  int    numCommands;
  int    commands[REG_INITIAL_NUM_CMDS];
	 
  int    num_recvd_cmds;
  int    recvd_cmds[REG_MAX_NUM_STR_CMDS];
  int    num_params_changed;
  char*  changed_param_labels[REG_MAX_NUM_STR_PARAMS];

  /* Some example variables */

  int   opacity_step_start = 120;
  int   opacity_step_stop = 130;
  float temp = 55.6;
  char* my_string = "running";

  int   finished;
  int   icmd;
  int   i, j;

  /*---------- End of declarations ------------*/

  for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){

    changed_param_labels[i]=(char *)malloc(REG_MAX_STRING_LENGTH*sizeof(char));

    if(changed_param_labels[i] == NULL){

      for(j=(i-1); j>=0; j--){
	free(changed_param_labels[j]);
      }
      printf("Failed to allocate memory for strings\n");
      return REG_FAILURE;
    }
  }

  /* Initialise & enable the steering library */

  Steering_enable(TRUE);

  numCommands = 2;
  commands[0] = REG_STR_STOP;
  commands[1] = REG_STR_PAUSE;
  status = Steering_initialize(numCommands, commands);

  if(status != REG_SUCCESS){
    return REG_FAILURE;
  }

  /* Register the input IO channel */

  iotype_labels[0] = "VTK_STRUCTURED_POINTS_INPUT";

  status = Register_IOTypes(1,
  			    iotype_labels,
  			    &input_iotype);

  if(status != REG_SUCCESS){

    printf("Failed to register IO types\n");
    return REG_FAILURE;
  }

  /* Register some parameters */

  param_labels[0] = "OPACITY_STEP_START";
  param_ptrs[0]   = (void *)(&opacity_step_start);
  param_types[0]  = REG_INT;
  param_strbl[0]  = TRUE;

  param_labels[1] = "OPACITY_STEP_STOP";
  param_ptrs[1]   = (void *)(&opacity_step_stop);
  param_types[1]  = REG_INT;
  param_strbl[1]  = TRUE;

  param_labels[2] = "TEMP";
  param_ptrs[2]   = (void *)(&temp);
  param_types[2]  = REG_FLOAT;
  param_strbl[2]  = FALSE;

  param_labels[3] = "A_STRING";
  param_ptrs[3]   = (void *)(my_string);
  param_types[3]  = REG_CHAR;
  param_strbl[3]  = TRUE;

  status = Register_params(4,
			   param_labels,
			   param_strbl,
			   param_ptrs,
			   param_types);

  if(status != REG_SUCCESS){

    printf("Failed to register parameters\n");
  }


  /* Enter main loop */

  for(i=0; i<8; i++){

    system("sleep 3");
    printf("\ni = %d\n", i);

    status = Steering_control(i,
			      &num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds);

    if(status == REG_SUCCESS){

      printf("opacity_step_start = %d\n", opacity_step_start);
      printf("opacity_step_sop   = %d\n", opacity_step_stop);
      printf("temp               = %f\n", temp);
      printf("my_string          = %s\n", my_string);

      if(num_recvd_cmds > 0){
  
    	printf("Received %d steerer cmds\n", num_recvd_cmds);
  
    	for(icmd=0; icmd<num_recvd_cmds; icmd++){
  
 	  switch (recvd_cmds[icmd]){
  
	  case REG_STR_PAUSE:
	    if(Steering_pause(&num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds) != REG_SUCCESS){

	      printf("Steering_pause returned error\n");
	    }
	    /* Reset loop to parse commands received following the
	       resume/stop command that broke us out of pause */
	    icmd = -1;
	    break;


 	  case REG_STR_STOP:
    	    finished = TRUE;
 	    break;

 	  default:

	    /* Deal with user-defined IO types etc. */

	    if(recvd_cmds[icmd] == input_iotype){

	      printf("Consume-input-file command received\n");
	    }
	    else{

	      printf("Received unrecognised steering command\n");
	    }
 	    break;
 	  }
  
 	  if(finished)break;
    	}
	if(finished)break;
      }
    }
    else{

      printf("Call to Steering_control failed\n");
    }

    /* Play with variables that are being monitored */
    temp += 0.5f;

  } /* End of main loop */

  status = Steering_finalize();

  /* Clean up */

  for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){

    free(changed_param_labels[i]);
  }

  return 0;
}
