/*---------------------------------------------------------------------------
  This file contains a very simple example of a steering-enabled 
  application.

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
#include "ReG_Steer_Appside.h"
#include <string.h>
#include <unistd.h>

/*-------------------------------------------------------------------------*/

int main(){

  /* No. of 'simulation' loops to do */
  const int nloops = 500000;

  /* For steering */
  int    status;
  int    numCommands;
  int    commands[REG_INITIAL_NUM_CMDS];
	 
  int    num_recvd_cmds;
  int    recvd_cmds[REG_MAX_NUM_STR_CMDS];
  char*  recvd_cmd_params[REG_MAX_NUM_STR_CMDS];
  int    num_params_changed;
  char*  changed_param_labels[REG_MAX_NUM_STR_PARAMS];

  float temp               = 55.6;
  int   finished           = REG_FALSE;
  int   icmd;
  int   i;
  
  /*---------- End of declarations ------------*/

  changed_param_labels[0] = (char *)malloc((REG_MAX_NUM_STR_CMDS+
		    REG_MAX_NUM_STR_PARAMS)*REG_MAX_STRING_LENGTH*sizeof(char));

  if(!changed_param_labels[0]){

    printf("Failed to allocate memory for strings\n");
    return REG_FAILURE;
  }

  for(i=1; i<REG_MAX_NUM_STR_PARAMS; i++){

    changed_param_labels[i]=changed_param_labels[i-1] + REG_MAX_STRING_LENGTH;
  }

  recvd_cmd_params[0] = changed_param_labels[REG_MAX_NUM_STR_PARAMS-1]
                     + REG_MAX_STRING_LENGTH;
  for(i=1; i<REG_MAX_NUM_STR_CMDS; i++){

    recvd_cmd_params[i] = recvd_cmd_params[i-1] + REG_MAX_STRING_LENGTH;
  }

  /* Initialise & enable the steering library */

  Steering_enable(REG_TRUE);

  numCommands = 2;
  commands[0] = REG_STR_STOP;
  commands[1] = REG_STR_PAUSE;
  status = Steering_initialize("simple v.1.0", numCommands, commands);

  if(status != REG_SUCCESS){
    return REG_FAILURE;
  }

  /* Register a steerable parameters */
  status = Register_param("TEMP", REG_TRUE, (void *)(&temp),
			  REG_FLOAT, "", "");
  if(status != REG_SUCCESS){
    printf("Failed to register parameter\n");
  }

  /* Enter main 'simulation' loop */

  for(i=0; i<nloops; i++){

    sleep(1); /* Pretend to do some work */

    printf("\ni = %d\n", i);

    /* Talk to the steering client (if one is connected) */
    status = Steering_control(i,
			      &num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds,
			      recvd_cmd_params);

    if(status == REG_SUCCESS){

      printf("temp  = %f\n", temp);

      if(num_recvd_cmds > 0){
  
    	printf("Received %d steerer cmds\n", num_recvd_cmds);
  
    	for(icmd=0; icmd<num_recvd_cmds; icmd++){
  
 	  switch (recvd_cmds[icmd]){
  
	  case REG_STR_PAUSE:
	    if(Steering_pause(&num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds,
			      recvd_cmd_params) != REG_SUCCESS){

	      printf("Steering_pause returned error\n");
	    }

	    /* Reset loop to parse commands received following the
	       resume/stop command that broke us out of pause */
	    icmd = -1;
	    break;


 	  case REG_STR_STOP:
    	    finished = REG_TRUE;
 	    break;

 	  default:

	    /* Deal with user-defined IO types etc.if any */
 	    break;
 	  }

	  /* Break out if steerer told us to stop */  
 	  if(finished)break; /* out of loop over rec'd commands */
    	}
	if(finished)break; /* out of main sim. loop */
      }
    }
    else{
      printf("Call to Steering_control failed\n");
    }
  } /* End of main loop */

  /* Clean up the steering library */
  status = Steering_finalize();

  /* Clean up locals */
  free(changed_param_labels[0]);

  return 0;
}

