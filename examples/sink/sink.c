/*---------------------------------------------------------------------------
  This file contains a very simple example of an application that
  reads data from an IO channel using the RealityGrid steering 
  library.

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
  const int nloops = 5000;

  /* For registering IOType */
  int    num_iotypes;
  int    iotype_handle[REG_INITIAL_NUM_IOTYPES];
  char  *iotype_labels[REG_INITIAL_NUM_IOTYPES];
  int	 iotype_dirn[REG_INITIAL_NUM_IOTYPES];
  int	 iotype_frequency[REG_INITIAL_NUM_IOTYPES];
  /* For receiving data */
  char   *c_array;
  int    *i_array;
  float  *f_array;
  double *d_array;

  /* For calling Steering_control */
  int    icmd;
  int    num_recvd_cmds;
  int    recvd_cmds[REG_MAX_NUM_STR_CMDS];
  char*  recvd_cmd_params[REG_MAX_NUM_STR_CMDS];
  int    num_params_changed;
  char*  changed_param_labels[REG_MAX_NUM_STR_PARAMS];
  int    finished = 0;

  REG_IOHandleType iohandle;
  int   data_type;
  int   data_count;
  int   status;
  int   numCommands;
  int   commands[REG_INITIAL_NUM_CMDS];
  int   sleep_time         = 1;
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

  Steering_enable(TRUE);

  numCommands = 1;
  commands[0] = REG_STR_STOP;
  status = Steering_initialize("sink v1.0", numCommands, commands);

  if(status != REG_SUCCESS){
    return REG_FAILURE;
  }

  /* Register the input IO channel */

  iotype_labels[0] = "VTK_STRUCTURED_POINTS";
  iotype_dirn[0] = REG_IO_IN;
  iotype_frequency[0] = 1; /* Attempt to consume data at every step */

  num_iotypes = 1;

  status = Register_IOTypes(num_iotypes,
  			    iotype_labels, 
			    iotype_dirn, 
			    iotype_frequency,
  			    iotype_handle);

  if(status != REG_SUCCESS){

    printf("Failed to register IO type\n");
    return REG_FAILURE;
  }

  /* Enter main loop waiting for data to arrive */

  for(i=0; i<nloops; i++){

    sleep(sleep_time);
    printf("\ni = %d\n", i);

    /* Talk to the steering client (if one is connected) */
    status = Steering_control(i,
			      &num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds,
			      recvd_cmd_params);

    if(status != REG_SUCCESS) continue;

    if(num_recvd_cmds > 0){

      for(icmd=0; icmd<num_recvd_cmds; icmd++){
  
	switch (recvd_cmds[icmd]){
  
	case REG_STR_STOP:
	  finished = 1;
	  break;

	default:
	  if(recvd_cmds[icmd] == iotype_handle[0]){

	    /* 'Open' the channel to consume data */
	    status = Consume_start(iotype_handle[0], &iohandle);

	    if( status == REG_SUCCESS ){

	      /* Data is available to read...get header describing it */
	      status = Consume_data_slice_header(iohandle, &data_type, 
						 &data_count);

	      while(status == REG_SUCCESS){

		printf("\nGot data: type = %d, count = %d\n", data_type, 
		       data_count);

		/* Read the data itself */
		switch(data_type){

		case REG_CHAR:

		  c_array = (char*)malloc(data_count*sizeof(char));

		  if(c_array){
		    status = Consume_data_slice(iohandle, data_type, 
						data_count, c_array);

		    printf("Got char data:\n>>%s<<\n", c_array);

		    free(c_array);
		  }
		  break;

		case REG_INT:

		  i_array = (int *)malloc(data_count*sizeof(int));

		  if(i_array){
		    status = Consume_data_slice(iohandle, data_type, 
						data_count, i_array);

		    printf("Got int data\n");
		    free(i_array);
		  }
		  break;

		case REG_FLOAT:

		  f_array = (float *)malloc(data_count*sizeof(float));

		  if(f_array){
		    status = Consume_data_slice(iohandle, data_type, 
						data_count, f_array);
		    printf("Got float data\n");
		    free(f_array);
		  }
		  break;

		case REG_DBL:

		  d_array = (double *)malloc(data_count*sizeof(double));

		  if(d_array){
		    status = Consume_data_slice(iohandle, data_type, 
						data_count, d_array);
		    printf("Got double data\n");
		    free(d_array);
		  }
		  break;

		default:
		  break;
		}

		status = Consume_data_slice_header(iohandle, &data_type,
						   &data_count);
	      }

	      /* Reached the end of this data set; 'close' the channel */
	      status = Consume_stop(&iohandle);
	    }
	  }
	  break;
	} /* end of switch statement */
	if(finished)break;
      } /* end loop over recvd cmds */
    } /* end if num_recvd_cmds > 0 */
    if(finished)break;
  } /* End of main loop */

  /* Clean-up the steering library */
  status = Steering_finalize();

  free(changed_param_labels[0]);
  return 0;
}

