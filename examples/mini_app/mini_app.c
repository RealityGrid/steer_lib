/*---------------------------------------------------------------------------
    This file contains a very simple example of a steering-enabled 
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
#include "ReG_Steer_Appside.h"

/*-------------------------------------------------------------------------*/

int main(){

  /* No. of 'simulation' loops to do */
  const int nloops = 100;

  /* For steering */
  int    num_iotypes;
  int    iotype_handle[REG_INITIAL_NUM_IOTYPES];
  char  *iotype_labels[REG_INITIAL_NUM_IOTYPES];
  int	 iotype_dirn[REG_INITIAL_NUM_IOTYPES];
  int	 iotype_frequency[REG_INITIAL_NUM_IOTYPES];
  int    num_chktypes;
  int    chktype_handle[REG_INITIAL_NUM_IOTYPES];
  char   chk_tag[REG_MAX_STRING_LENGTH];
  REG_IOHandleType iohandle;
  int    data_type;
  int    data_count;

  char  *param_labels[REG_INITIAL_NUM_PARAMS];
  void  *param_ptrs[REG_INITIAL_NUM_PARAMS];
  int    param_types[REG_INITIAL_NUM_PARAMS];
  int    param_strbl[REG_INITIAL_NUM_PARAMS];
	 
  int    status;
  int    numCommands;
  int    commands[REG_INITIAL_NUM_CMDS];
	 
  int    num_recvd_cmds;
  int    recvd_cmds[REG_MAX_NUM_STR_CMDS];
  char*  recvd_cmd_params[REG_MAX_NUM_STR_CMDS];
  int    num_params_changed;
  char*  changed_param_labels[REG_MAX_NUM_STR_PARAMS];

  /* Some example variables */

  int   opacity_step_start = 120;
  int   opacity_step_stop  = 130;
  int   output_freq        = 5;
  float temp               = 55.6;
  char  my_string[REG_MAX_STRING_LENGTH];

  int   finished           = FALSE;
  int   icmd;
  int   i, j;
  const int NX = 16;
  const int NY = 16;
  const int NZ = 16;
  
  double aaxis = 1.5;
  double baxis = 1.5;
  double caxis = 1.5;

  float *array;
  char   header[BUFSIZ];

  /*FILE *fp;*/

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

  numCommands = 2;
  commands[0] = REG_STR_STOP;
  commands[1] = REG_STR_PAUSE;
  status = Steering_initialize(numCommands, commands);

  if(status != REG_SUCCESS){
    return REG_FAILURE;
  }

  /* Register the input IO channel */

  iotype_labels[0] = "VTK_STRUCTURED_POINTS_INPUT";
  iotype_dirn[0] = REG_IO_IN;
  iotype_frequency[0] = 0;

  iotype_labels[1] = "VTK_OUTPUT_GLOBUS_IO";
  iotype_dirn[1] = REG_IO_OUT;
  iotype_frequency[1] = output_freq;

  num_iotypes = 2;

  status = Register_IOTypes(num_iotypes,
  			    iotype_labels, 
			    iotype_dirn, 
			    iotype_frequency,
  			    iotype_handle);

  if(status != REG_SUCCESS){

    printf("Failed to register IO types\n");
    return REG_FAILURE;
  }

  /* Register checkpoint emission */
  iotype_labels[0] = "MY_CHECKPOINT";
  iotype_dirn[0] = REG_IO_OUT;
  iotype_frequency[0] = 0;

  iotype_labels[1] = "MY_OTHER_CHECKPOINT";
  iotype_dirn[1] = REG_IO_INOUT;
  iotype_frequency[1] = 2;

  iotype_labels[2] = "YET_ANOTHER_CHECKPOINT";
 iotype_dirn[2] = REG_IO_INOUT;
  iotype_frequency[2] = 1;

  num_chktypes = 3;

  status = Register_ChkTypes(num_chktypes,
  			    iotype_labels, 
			    iotype_dirn, 
			    iotype_frequency,
  			    chktype_handle);

  if(status != REG_SUCCESS){

    printf("Failed to register Chk types\n");
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
  sprintf(my_string, "running");
  param_ptrs[3]   = (void *)(my_string);
  param_types[3]  = REG_CHAR;
  param_strbl[3]  = TRUE;

  param_labels[4] = "a_axis";
  param_ptrs[4]   = (void *)(&aaxis);
  param_types[4]  = REG_DBL;
  param_strbl[4]  = TRUE;

  param_labels[5] = "b_axis";
  param_ptrs[5]   = (void *)(&baxis);
  param_types[5]  = REG_DBL;
  param_strbl[5]  = TRUE;

  param_labels[6] = "c_axis";
  param_ptrs[6]   = (void *)(&caxis);
  param_types[6]  = REG_DBL;
  param_strbl[6]  = TRUE;

   status = Register_params(7,
			   param_labels,
			   param_strbl,
			   param_ptrs,
			   param_types);

  if(status != REG_SUCCESS){

    printf("Failed to register parameters\n");
  }

  /* malloc memory for array */

  array = (float *)malloc(NX*NY*NZ*sizeof(float));
  if( !array ){

    fprintf(stderr, "Malloc failed...\n");
    return REG_FAILURE;
  }


  /* Enter main loop */

  for(i=0; i<nloops; i++){

    system("sleep 3");
    printf("\ni = %d\n", i);

    status = Steering_control(i,
			      &num_params_changed,
			      changed_param_labels,
			      &num_recvd_cmds,
			      recvd_cmds,
			      recvd_cmd_params);

    if(status == REG_SUCCESS){

      printf("opacity_step_start = %d\n", opacity_step_start);
      printf("opacity_step_stop  = %d\n", opacity_step_stop);
      printf("temp               = %f\n", temp);
      printf("my_string          = %s\n", my_string);
      printf("output_freq        = %d\n", output_freq);

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
    	    finished = TRUE;
 	    break;

 	  default:

	    /* Deal with user-defined IO types etc. */

	    for(j=0; j<num_iotypes; j++){

	      if(recvd_cmds[icmd] == iotype_handle[j]){

	        printf("Some IO command received\n");

		if(j==1){

		  if(Make_vtk_header(header, "Some data", NX, NY, NZ, 1, 
				     REG_FLOAT) != REG_SUCCESS) {
		    continue;
		  }
		  if(Make_vtk_buffer(NX, NY, NZ, 1, aaxis, baxis, caxis, 
                                     array)
		     != REG_SUCCESS){

		    continue;
		  }

		  if( Emit_start(iotype_handle[j], i, TRUE, &iohandle)
		      == REG_SUCCESS ){


		    data_count = strlen(header);
		    data_type  = REG_CHAR;
		    status = Emit_data_slice(iohandle, data_type, data_count, 
					     (void *)header);

		    if(status != REG_SUCCESS){

		      printf("Call to Emit_data_slice failed\n");
		      Emit_stop(&iohandle);
		      continue;
		    }

		    data_count = NX*NY*NZ;
		    data_type  = REG_FLOAT;
		    status = Emit_data_slice(iohandle, data_type, data_count, 
					     array);

		    Emit_stop(&iohandle);
		  }
		}
	        break;
	      }
	    }
	    for(j=0; j<num_chktypes; j++){

	      if(recvd_cmds[icmd] == chktype_handle[j]){

		printf("Got checkpoint command, parameters: %s\n", 
		       recvd_cmd_params[icmd]);

		/* Pretend we've taken a checkpoint here */
		sprintf(chk_tag, "checkpoint_%d.dat", i);
		Record_Chkpt(chktype_handle[j], chk_tag);
		break;
	      }
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
  free(changed_param_labels[0]);
  free(array);

  return 0;
}

