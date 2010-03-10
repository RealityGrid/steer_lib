/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
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

  Author: Andrew Porter
          Robert Haines
 */

/** @file mini_app.c
    @brief An example of a steering-enabled application using most of
    the steering lib's features.

    @author Andrew Porter
    @author Robert Haines */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Appside.h"

/*-------------------------------------------------------------------------*/

void update_data(int, double, int, float**);

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

  int    status;
  int    numCommands;
  int    commands[REG_INITIAL_NUM_CMDS];
  int    num_recvd_cmds;
  int    recvd_cmds[REG_MAX_NUM_STR_CMDS];
  char** recvd_cmd_params;
  int    num_params_changed;
  char** changed_param_labels;

  /* Some example variables */
  int    sleep_time = 200;
  char   my_string[REG_MAX_STRING_LENGTH];
  int    side_length = 100;
  int    old_side_length = 0;
  int    amplitude = 12;
  double multiplier = 1.0;
  double multiplier_inc = -0.1;
  float* data_array = NULL;

  int   itag;
  int   finished = REG_FALSE;
  int   icmd;
  int   i, j;

  FILE  *fp;

  /*---------- End of declarations ------------*/

  /* Use library utility routines to allocate arrays of strings
     for passing in to Steering_control */
  changed_param_labels = Alloc_string_array(REG_MAX_STRING_LENGTH,
					    REG_MAX_NUM_STR_PARAMS);
  recvd_cmd_params = Alloc_string_array(REG_MAX_STRING_LENGTH,
					REG_MAX_NUM_STR_CMDS);

  if(!changed_param_labels || !recvd_cmd_params) {
    printf("Failed to allocate string arrays.\n");
    return REG_FAILURE;
  }

  /* Initialise & enable the steering library */

  Steering_enable(REG_TRUE);

  numCommands = 2;
  commands[0] = REG_STR_STOP;
  commands[1] = REG_STR_PAUSE;
  status = Steering_initialize("mini_app v2.5", numCommands, commands);

  if(status != REG_SUCCESS) {
    return REG_FAILURE;
  }

  /* Register the input and output IO channels */

  if(Register_IOType("SOME_INPUT_DATA",
		     REG_IO_IN,
		     0, /* Don't do any auto consumption */
		     &(iotype_handle[0])) != REG_SUCCESS) {
    printf("Failed to register IO type SOME_INPUT_DATA\n");
    Steering_finalize();
    return REG_FAILURE;
  }
  if(Register_IOType("mini_app visualization data",
		     REG_IO_OUT,
		     1, /* Attempt to do output every timestep */
		     &(iotype_handle[1])) != REG_SUCCESS) {
    printf("Failed to register IO type 'mini_app visualization data'\n");
    Steering_finalize();
    return REG_FAILURE;
  }
  num_iotypes = 2;

  /* Register checkpoint emission */

  if(Register_ChkType("MY_CHECKPOINT",
		      REG_IO_OUT,
		      0, /* No auto checkpointing */
		      &(chktype_handle[0])) != REG_SUCCESS) {
    printf("Failed to register Chk type MY_CHECKPOINT\n");
    return REG_FAILURE;
  }

  if(Register_ChkType("MY_OTHER_CHECKPOINT",
		      REG_IO_INOUT,
		      0, /* No auto checkpointing */
		      &(chktype_handle[1])) != REG_SUCCESS) {
    printf("Failed to register Chk type MY_OTHER_CHECKPOINT\n");
    return REG_FAILURE;
  }

  if(Register_ChkType("YET_ANOTHER_CHECKPOINT",
		      REG_IO_INOUT,
		      0, /* No auto checkpointing */
		      &(chktype_handle[2])) != REG_SUCCESS) {
    printf("Failed to register Chk type YET_ANOTHER_CHECKPOINT\n");
    return REG_FAILURE;
  }
  num_chktypes = 3;

  /* Register some parameters */

  sprintf(my_string, "running");
  status = Register_param("a string", REG_TRUE, (void *)(my_string),
			  REG_CHAR, "", "10");
  status = Register_param("time_to_sleep", REG_TRUE, (void *)(&sleep_time),
			  REG_INT, "100", "1000");
  status = Register_param("data side length", REG_TRUE, (void *)(&side_length),
			  REG_INT, "10", "500");
  status = Register_param("max amplitude", REG_TRUE, (void *)(&amplitude),
			  REG_INT, "1", "15");
  status = Register_param("multiplier", REG_FALSE, (void *)(&multiplier),
			  REG_DBL, "", "");

  if(status != REG_SUCCESS) {
    printf("Failed to register parameters\n");
  }

  /* Enter main loop */

  for(i=0; i<nloops; i++) {

    /* Pretend to do lots of work */
    usleep(sleep_time * 1000);

    /* check that memory block is big enough */
    if(side_length > old_side_length) {
      if(data_array) free(data_array);
      data_array = (float*) malloc(sizeof(float) * side_length * side_length);
    }
    update_data(amplitude, multiplier, side_length, &data_array);
    old_side_length = side_length;

    /* alter multiplier */
    multiplier += multiplier_inc;
    if(multiplier > 1.0) {
      multiplier = 1.0;
      multiplier_inc = -0.1;
    }
    if(multiplier < -1.0) {
      multiplier = -1.0;
      multiplier_inc = 0.1;
    }

    printf("\ni = %d\n", i);
    printf("my_string          = %s\n", my_string);
    printf("data side length   = %d\n", side_length);
    printf("maximum amplitude  = %d\n", amplitude);
    printf("multiplier param   = %f\n", multiplier);

    /* Talk to the steering client (if one is connected) */
    status = Steering_control(i,
			      &num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds,
			      recvd_cmd_params);

    if(status == REG_SUCCESS) {

      if(num_recvd_cmds > 0) {

    	printf("Received %d steerer cmds\n", num_recvd_cmds);

    	for(icmd=0; icmd<num_recvd_cmds; icmd++) {

 	  switch(recvd_cmds[icmd]) {
	  case REG_STR_PAUSE:
	    if(Steering_pause(&num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds,
			      recvd_cmd_params) != REG_SUCCESS) {

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

	    for(j=0; j<num_iotypes; j++) {

	      if(recvd_cmds[icmd] == iotype_handle[j]) {

	        printf("Some IO command received\n");

		/* must ensure that data size has not been steered
		   before the data has been re-allocated! */
		if((j == 1) && (side_length == old_side_length)) {

		  /* We've been told to emit some data */
		  if(Emit_start(iotype_handle[j], i, &iohandle)
		      == REG_SUCCESS) {

		    /* Emit dimensions of data */
		    status = Emit_data_slice(iohandle, REG_INT, 1,
					     (void*) &side_length);

		    /* Emit data */
		    status = Emit_data_slice(iohandle, REG_FLOAT,
					     (side_length * side_length),
					     (void*) data_array);

		    Emit_stop(&iohandle);
		  } // Emit_start
		} // j == 1
	        break;
	      }
	    }

	    /* Check for a command to checkpoint or restart */
	    for(j=0; j<num_chktypes; j++) {

	      if(recvd_cmds[icmd] == chktype_handle[j]) {

		printf("Got checkpoint command, parameters >>%s<<\n",
		       recvd_cmd_params[icmd]);

		if(strstr(recvd_cmd_params[icmd], "OUT")) {
		  /* Pretend we've taken a checkpoint here */
		  itag = rand();
		  sprintf(chk_tag, "fake_chkpoint_%d.dat", itag);

		  /* Add this filename to the record of the checkpoint */
		  Add_checkpoint_file(chktype_handle[j], chk_tag);

		  if((fp = fopen(chk_tag, "w"))) {
		    fprintf(fp, "Chkpoint data goes here\n");
		    fclose(fp);
		    sprintf(chk_tag,"%d", itag);
		    /* Record that we've taken the checkpoint */
		    Record_checkpoint_set(chktype_handle[j], chk_tag, ".");
		  }
		}
		break;
	      }
	    }
 	    break;
 	  }

	  /* Break out if steerer told us to stop */
 	  if(finished) break;
    	}
	if(finished) break;
      }
    }
    else {
      printf("Call to Steering_control failed\n");
    }

  } /* End of main loop */

  /* Clean up the steering library */
  status = Steering_finalize();

  return 0;
}

void update_data(int amp, double multi, int size, float** data) {
  float q, x, y, s, t;
  float start = (float) -M_PI;
  float incr = (-2.0f * start) / size;
  int i, j, index;

  for(i = 0; i < size; i++) {
    x = start + (incr * i);
    s = amp * (float) multi * cosf(x * 5);
    for(j = 0; j < size; j++) {
      y = start + (incr * j);
      t = amp * (float) multi * cosf(y * 4);
      index = (i * size) + j;
      q = (x * x) + (y * y);
      (*data)[index] = expf(q * -0.5f) * (s + t);
    }
  }
}
