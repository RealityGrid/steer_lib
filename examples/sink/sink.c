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

/** @file sink.c
    @brief A very simple example of an application that
    reads data from an IO channel using the RealityGrid steering
    library.
    @author Andrew Porter
    @author Robert Haines */

#include "ReG_Steer_Config.h"
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
  long   *l_array;
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
  int   i, j;
  /* Monitored parameter */
  int   bytes_read = 0;

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

  numCommands = 1;
  commands[0] = REG_STR_STOP;
  status = Steering_initialize("sink v1.0", numCommands, commands);

  if(status != REG_SUCCESS){
    printf("sink: call to Steering_initialize failed - quitting\n");
    return REG_FAILURE;
  }

  /* Register the input IO channel */

  iotype_labels[0] = "mini_app visualization data";
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

  status = Register_param("Bytes_read", REG_FALSE,
			  (void *)(&bytes_read), REG_INT, "", "");

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

    /* Zero count of bytes read this time around */
    bytes_read = 0;

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

		printf("\nGot data - ");

		/* Read the data itself */
		switch(data_type){

		case REG_CHAR:

		  c_array = (char*)malloc(data_count*sizeof(char));

		  if(c_array){
		    status = Consume_data_slice(iohandle, data_type,
						data_count, c_array);

		    printf("type: char, number: %d\n", data_count);

		    bytes_read += data_count*sizeof(char);
		    free(c_array);
		  }
		  break;

		case REG_INT:

		  i_array = (int *)malloc(data_count*sizeof(int));

		  if(i_array){
		    status = Consume_data_slice(iohandle, data_type,
						data_count, i_array);

		    printf("type: int, number: %d\n", data_count);

		    bytes_read += data_count*sizeof(int);
		    free(i_array);
		  }
		  break;

		case REG_LONG:

		  l_array = (long *)malloc(data_count*sizeof(long));

		  if(l_array){
		    status = Consume_data_slice(iohandle, data_type,
						data_count, l_array);

		    printf("type: long, number: %d\n", data_count);

		    bytes_read += data_count*sizeof(int);
		    free(i_array);
		  }
		  break;

		case REG_FLOAT:

		  f_array = (float *)malloc(data_count*sizeof(float));

		  if(f_array){
		    status = Consume_data_slice(iohandle, data_type,
						data_count, f_array);

		    printf("type: float, number: %d\n", data_count);

		    bytes_read += data_count*sizeof(float);
		    free(f_array);
		  }
		  break;

		case REG_DBL:

		  d_array = (double *)malloc(data_count*sizeof(double));

		  if(d_array){
		    status = Consume_data_slice(iohandle, data_type,
						data_count, d_array);

		    printf("type: double, number: %d\n", data_count);

		    bytes_read += data_count*sizeof(double);
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
