/*---------------------------------------------------------------------------
    This file contains a very simple example of an application that
    reads data from an IO channel using the RealityGrid steering 
    library.

    (C)Copyright 2004 The University of Manchester, United Kingdom,
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

    Initial version by:  A Porter, 18.3.2004

---------------------------------------------------------------------------*/
#include "ReG_Steer_Appside.h"
#include <string.h>
#include <unistd.h>

/*-------------------------------------------------------------------------*/

int main(){

  /* No. of 'simulation' loops to do */
  const int nloops = 500;

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

  REG_IOHandleType iohandle;
  int   data_type;
  int   data_count;
  int   status;
  int   numCommands;
  int   commands[REG_INITIAL_NUM_CMDS];
  int   sleep_time         = 1;
  int   i;

  /*---------- End of declarations ------------*/

  /* Initialise & enable the steering library */

  Steering_enable(TRUE);

  numCommands = 0;
  status = Steering_initialize(numCommands, commands);

  if(status != REG_SUCCESS){
    return REG_FAILURE;
  }

  /* Register the input IO channel */

  iotype_labels[0] = "VTK_STRUCTURED_POINTS";
  iotype_dirn[0] = REG_IO_IN;
  iotype_frequency[0] = 0;

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

    /* 'Open' the channel to consume data */
    status = Consume_start(iotype_handle[0], &iohandle);

    if( status == REG_SUCCESS ){

      /* Data is available to read...get header describing it */
      status = Consume_data_slice_header(iohandle, &data_type, &data_count);

      while(status == REG_SUCCESS){

	printf("\nGot data: type = %d, count = %d\n", data_type, data_count);

	/* Read the data itself */
	switch(data_type){

	case REG_CHAR:

	  c_array = (char*)malloc(data_count*sizeof(char));

	  if(c_array){
	    status = Consume_data_slice(iohandle, data_type, data_count,
					c_array);

	    printf("Got char data:\n>>%s<<\n", c_array);

	    free(c_array);
	  }
	  break;

	case REG_INT:

	  i_array = (int *)malloc(data_count*sizeof(int));

	  if(i_array){
	    status = Consume_data_slice(iohandle, data_type, data_count,
					i_array);

	    printf("Got int data\n");
	    free(i_array);
	  }
	  break;

	case REG_FLOAT:

	  f_array = (float *)malloc(data_count*sizeof(float));

	  if(f_array){
	    status = Consume_data_slice(iohandle, data_type, data_count,
					f_array);
	    printf("Got float data\n");
	    free(f_array);
	  }
	  break;

	case REG_DBL:

	  d_array = (double *)malloc(data_count*sizeof(double));

	  if(d_array){
	    status = Consume_data_slice(iohandle, data_type, data_count,
					d_array);
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
    
  } /* End of main loop */

  /* Clean-up the steering library */
  status = Steering_finalize();

  return 0;
}

