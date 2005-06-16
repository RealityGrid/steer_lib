/*---------------------------------------------------------------------------
  This file contains an example of a steering-enabled 
  application using most of the steering lib's features.

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
#include "ReG_Steer_Appside.h"
#include <string.h>
#include <unistd.h>

/*-------------------------------------------------------------------------*/

int main(){

  /* No. of 'simulation' loops to do */
  const int nloops = 500000;

  /* For steering */
  int    num_iotypes;
  int    iotype_handle[REG_INITIAL_NUM_IOTYPES];
  int    num_chktypes;
  int    chktype_handle[REG_INITIAL_NUM_IOTYPES];
  char   chk_tag[REG_MAX_STRING_LENGTH];
  REG_IOHandleType iohandle;
  int    data_type;
  int    data_count;
	 
  int    status;
  int    numCommands;
  int    commands[REG_INITIAL_NUM_CMDS];
  int    num_recvd_cmds;
  int    recvd_cmds[REG_MAX_NUM_STR_CMDS];
  char** recvd_cmd_params;
  int    num_params_changed;
  char** changed_param_labels;

  /* Some example variables */

  int   opacity_step_stop  = 130;
  int   sleep_time         = 1;
  float temp               = 55.6;
  char  my_string[REG_MAX_STRING_LENGTH];
  int   nx = 16;
  int   ny = 16;
  int   nz = 16;

  int   itag;
  int   finished = REG_FALSE;
  int   icmd;
  int   i, j;
  
  double aaxis = 1.5;
  double baxis = 1.5;
  double caxis = 1.5;

  float *array;
  char   header[BUFSIZ];
  int    ichunk, nchunk;
  int    chunk_dim = 4;

  FILE  *fp;

  /*---------- End of declarations ------------*/

  /* Use library utility routines to allocate arrays of strings
     for passing in to Steering_control */
  changed_param_labels = Alloc_string_array(REG_MAX_STRING_LENGTH,
					    REG_MAX_NUM_STR_PARAMS);
  recvd_cmd_params = Alloc_string_array(REG_MAX_STRING_LENGTH,
					REG_MAX_NUM_STR_CMDS);

  if(!changed_param_labels || !recvd_cmd_params){
    printf("Failed to allocate string arrays :-(\n");
    return REG_FAILURE;
  }

  /* Initialise & enable the steering library */

  Steering_enable(REG_TRUE);

  numCommands = 2;
  commands[0] = REG_STR_STOP;
  commands[1] = REG_STR_PAUSE;
  status = Steering_initialize("mini_app v.1.0", numCommands, commands);

  if(status != REG_SUCCESS){
    return REG_FAILURE;
  }

  /* Register the input and output IO channels */

  if( Register_IOType("SOME_INPUT_DATA", 
		      REG_IO_IN, 
		      0, /* Don't do any auto consumption */
		      &(iotype_handle[0])) != REG_SUCCESS){
    printf("Failed to register IO type SOME_INPUT_DATA\n");
    Steering_finalize();
    return REG_FAILURE;
  }
  if( Register_IOType("VTK_STRUCTURED_POINTS",
		      REG_IO_OUT, 
		      1, /* Attempt to do output every timestep */
		      &(iotype_handle[1])) != REG_SUCCESS){
    printf("Failed to register IO type VTK_STRUCTURED_POINTS\n");
    Steering_finalize();
    return REG_FAILURE;
  }
  num_iotypes = 2;

  /* Register checkpoint emission */

  if( Register_ChkType("MY_CHECKPOINT", 
		       REG_IO_OUT, 
		       0, /* No auto checkpointing */
		       &(chktype_handle[0])) != REG_SUCCESS){
    printf("Failed to register Chk type MY_CHECKPOINT\n");
    return REG_FAILURE;
  }

  if( Register_ChkType("MY_OTHER_CHECKPOINT", 
		       REG_IO_INOUT, 
		       0, /* No auto checkpointing */
		       &(chktype_handle[1])) != REG_SUCCESS){
    printf("Failed to register Chk type MY_OTHER_CHECKPOINT\n");
    return REG_FAILURE;
  }

  if( Register_ChkType("YET_ANOTHER_CHECKPOINT", 
		       REG_IO_INOUT, 
		       0, /* No auto checkpointing */
		       &(chktype_handle[2])) != REG_SUCCESS){
    printf("Failed to register Chk type YET_ANOTHER_CHECKPOINT\n");
    return REG_FAILURE;
  }
  num_chktypes = 3;

  /* Register some parameters */

  status = Register_param("OPACITY_STEP_STOP", REG_TRUE, 
			  (void *)(&opacity_step_stop),
			  REG_INT, "0", "256");
  status = Register_param("TEMP", REG_FALSE, (void *)(&temp),
			  REG_FLOAT, "", "");
  sprintf(my_string, "running");
  status = Register_param("A_STRING", REG_TRUE, (void *)(my_string),
			  REG_CHAR, "", "10");
  status = Register_param("a_axis", REG_TRUE, (void *)(&aaxis),
			  REG_DBL, "0.01", "10.0");
  status = Register_param("b_axis", REG_TRUE, (void *)(&baxis),
			  REG_DBL, "0.01", "10.0");
  status = Register_param("c_axis", REG_TRUE, (void *)(&caxis),
			  REG_DBL, "0.01", "10.0");
  status = Register_param("time_to_sleep", REG_TRUE, (void *)(&sleep_time),
			  REG_INT, "0", "100");
  status = Register_param("nx", REG_TRUE, (void *)(&nx),
			  REG_INT, "1", "");
  status = Register_param("ny", REG_TRUE, (void *)(&ny),
			  REG_INT, "1", "");
  status = Register_param("nz", REG_TRUE, (void *)(&nz),
			  REG_INT, "1", "");
  if(status != REG_SUCCESS){
    printf("Failed to register parameters\n");
  }

  /* Enter main loop */

  for(i=0; i<nloops; i++){

    sleep(sleep_time); /* Pretend to do some work */
    /* usleep(sleep_time*1000000); */

    printf("\ni = %d\n", i);

    /* Talk to the steering client (if one is connected) */
    status = Steering_control(i,
			      &num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds,
			      recvd_cmd_params);

    if(status == REG_SUCCESS){

      printf("opacity_step_stop  = %d\n", opacity_step_stop);
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

	    /* Deal with user-defined IO types etc. */

	    for(j=0; j<num_iotypes; j++){

	      if(recvd_cmds[icmd] == iotype_handle[j]){

	        printf("Some IO command received\n");

		if(j==1){

		  /* We've been told to emit some data */
		  if( Emit_start(iotype_handle[j], i, &iohandle)
		      == REG_SUCCESS ){

		    /* Make the vtk header to describe the data and then
		       emit it */
		    if(Make_vtk_header(header, "Some data", nx, ny, nz, 1, 
				       REG_FLOAT) != REG_SUCCESS) {
		      continue;
		    }

		    printf("First slice...\n");
		    data_count = strlen(header);
		    data_type  = REG_CHAR;
		    status = Emit_data_slice(iohandle, data_type, data_count, 
					     (void *)header);

		    if(status != REG_SUCCESS){

		      printf("Call to Emit_data_slice failed\n");
		      Emit_stop(&iohandle);
		      continue;
		    }

		    /* malloc memory for array; nx, ny and nz are steerable
		       so we malloc everytime.  Could be done with a realloc
		       but it's not necessary here. */

		    if( !(array = (float *)malloc(nx*ny*nz*sizeof(float))) ){
		    
		      fprintf(stderr, "Malloc of %d bytes failed...\n",
			      (int)(nx*ny*nz*sizeof(float)));
		      status = Steering_finalize();
		      return REG_FAILURE;
		    }

		    /* Make a dummy data set of 3D volumetric data */
		    if(Make_vtk_buffer(nx, ny, nz, 1, aaxis, baxis, caxis, 
				       array)
		       != REG_SUCCESS){

		      free(array);
		      array = NULL;
		      continue;
		    }

		    /* Emit the data set in chunks to mimic situation
		       when data collected process by process in a parallel
		       program */
		    if(nx % chunk_dim != 0){

		      printf("nx not a multiple of %d\n", chunk_dim);
		      free(array);
		      array = NULL;
		      Emit_stop(&iohandle);
		      continue;
		    }

		    nchunk = nx/chunk_dim;
		    printf("nx = %d, chunk_dim = %d so have %d chunks...\n",
			   nx, chunk_dim, nchunk);

		    for(ichunk=0; ichunk<nchunk; ichunk++){

		      printf("chunk %d...\n", ichunk);

		      /* Construct header for this chunk to allow the recipient 
			 of this data to reconstruct the data set */
		      status = Make_chunk_header(header, iohandle, nx, ny, nz, 
						 (ichunk*chunk_dim), 0, 0, 
						 chunk_dim, ny, nz);

		      data_count = strlen(header);
		      data_type  = REG_CHAR;
		      status = Emit_data_slice(iohandle, data_type, data_count,
					       &header);
		      if(status != REG_SUCCESS){
			printf("Emit_data_slice failed - end emit\n");
			break;
		      }

		      data_count = chunk_dim*ny*nz;
		      data_type  = REG_FLOAT;
		      status = Emit_data_slice(iohandle, data_type, data_count, 
					       &(array[(ichunk*chunk_dim)*ny*nz]));
		      if(status != REG_SUCCESS){
			printf("Emit_data_slice failed - end emit\n");
			break;
		      }
		    }

		    Emit_stop(&iohandle);
		    free(array);
		    array = NULL;
		  }
		}
	        break;
	      }
	    }
	    /* Check for a command to checkpoint or restart */
	    for(j=0; j<num_chktypes; j++){

	      if(recvd_cmds[icmd] == chktype_handle[j]){

		printf("Got checkpoint command, parameters: %s\n", 
		       recvd_cmd_params[icmd]);

		if(strstr(recvd_cmd_params[icmd], "OUT")){
		  /* Pretend we've taken a checkpoint here */
		  itag = rand();
		  sprintf(chk_tag, "fake_chkpoint_%d.dat", itag);

		  /* Add this filename to the record of the checkpoint */
		  Add_checkpoint_file(chktype_handle[j], chk_tag);

		  if( (fp = fopen(chk_tag, "w")) ){
		    fprintf(fp, "Chkpoint data goes here\n");
		    fclose(fp);
		    sprintf(chk_tag,"%d", itag);
		    /* Record that we've taken the checkpoint */
		    Record_checkpoint_set(chktype_handle[j],
					  chk_tag,  ".");
		  }
		}
		break;
	      }
	    }
 	    break;
 	  }

	  /* Break out if steerer told us to stop */  
 	  if(finished)break;
    	}
	if(finished)break;
      }
    }
    else{

      printf("Call to Steering_control failed\n");
    }

    /* Play with variables that are being monitored */
    temp += 0.5347672f;

  } /* End of main loop */

  /* Clean up the steering library */
  status = Steering_finalize();

  return 0;
}

