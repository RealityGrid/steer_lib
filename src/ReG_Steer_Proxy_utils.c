/*----------------------------------------------------------------------------
    Library routines and associated data structures for use in a 
    creating and communicating with a steering proxy process (as
    defined in SteeringProxy.java).

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
    30.9.2002       0.1                                         A Porter

---------------------------------------------------------------------------*/

#include "ReG_Steer_Proxy_utils.h"

#ifndef DEBUG
#define DEBUG 1
#endif

/*----------------------------------------------------------*/

int Create_proxy(int *to_proxy, int *from_proxy)
{
  const int stdin_fd  = 0;
  const int stdout_fd = 1;

  int   status;
  pid_t my_id;

  int   pipe_to_proxy[2];
  int   pipe_from_proxy[2];

  int   nbytes;
  char  msg[MAX_MSG_SIZE];

  char *registry_gsh;
  char  registry_arg[REG_MAX_STRING_LENGTH];
  /* ARPDBG - exec_path should not be hardwired... */
  /* char *exec_path = "/home/bezier1/zzcguap/bin/SteererProxy"; */
  char *exec_path = "ReG_Steer_Proxy";
  char *class_path;

  /* Get the value of the CLASSPATH environment variable for launching
     the java proxy */
  if( (class_path = getenv("CLASSPATH")) == NULL){

    fprintf(stderr, "Failed to get CLASSPATH env. variable\n");
    return REG_FAILURE;
  }

  /* Get the value of the REGISTRY_GSH environment variable - this gives
     the grid service handle of the registry that the proxy is to
     bind to */
  if( (registry_gsh = getenv("REGISTRY_GSH")) == NULL){

    fprintf(stderr, "Failed to get REGISTRY_GSH env. variable\n");
    return REG_FAILURE;
  }

  sprintf(registry_arg, "-Dregistry.gsh=%s", registry_gsh);

  /* Create pipe for sending messages _to_ proxy */
  if(pipe(pipe_to_proxy)){

    fprintf(stderr, "Create_proxy: failed to create pipe_to_proxy...\n");
    return REG_FAILURE;
  }

  *to_proxy = pipe_to_proxy[1];

  /* Create pipe for receiving messages _from_ proxy */
  if(pipe(pipe_from_proxy)){

    fprintf(stderr, "Create_proxy: failed to create pipe_from_proxy...\n");
    return REG_FAILURE;
  }

  *from_proxy = pipe_from_proxy[0];

  /* Launch new child thread */
  my_id = fork();

  if(my_id == -1){

    fprintf(stderr, "Create_proxy: call to fork() failed...\n");
    return REG_FAILURE;
  }
  else if(my_id == 0){

    /* Am newly-created thread */

    /* Close the Child process' STDIN */
    close(stdin_fd);

    /* Duplicate the Child's STDIN to the pipe_to_proxy[0] file descriptor - 
       dup always binds to the lowest available file descriptor which
       is the one we just freed via the call to close... */
    if( (status = dup(pipe_to_proxy[0])) != stdin_fd){

      fprintf(stderr, "Failed to attach to stdin, status = %d\n", status);
      Send_proxy_message(pipe_from_proxy[1], "ERROR");
      exit(1);
    }

    /* Close the Child process' STDOUT */
    close(stdout_fd);

    /* Duplicate the Child's STDOUT to the pipe_from_proxy[1] file 
       descriptor */
    if( (status = dup(pipe_from_proxy[1])) != stdout_fd){

      fprintf(stderr, "Failed to attach to stdout, status = %d\n", status);
      Send_proxy_message(pipe_from_proxy[1], "ERROR");
      exit(1);
    }

    /*  Make the exec call to run the javac program. */
    execlp("java","java", "-Xmx20m", registry_arg, 
	   "-classpath", class_path, exec_path, (char *)0);

    /* If execlp returned then it failed... */
    fprintf(stderr, "Create_proxy: execv failed...\n");

    Send_proxy_message(pipe_from_proxy[1], "ERROR");
    exit(1);
  }
  else{
    /* Am parent process */

    /* Check that proxy has launched OK */
    Get_proxy_message(pipe_from_proxy[0], msg, &nbytes);

    if(strcmp(msg, "ERROR") == 0){

      return REG_FAILURE;
    }
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Destroy_proxy(int pipe_to_proxy)
{
  return Send_proxy_message(pipe_to_proxy, "QUIT");
}

/*----------------------------------------------------------*/

int Send_proxy_message(int pipe_to_proxy, const char *buf)
{
  int  nbytes;
  int  buf_len;
  /*  char len[HEADER_BYTES]; */
  char *len;

  /* Send how many bytes... */
  buf_len = strlen(buf);
  len = (char *) &buf_len;

  /*  sprintf(len, "%d", buf_len); */
 
  nbytes = write(pipe_to_proxy, len, HEADER_BYTES);

  if(nbytes != HEADER_BYTES){

    fprintf(stderr, "Send_proxy_message: error writing to pipe\n");
    return REG_FAILURE;
  }

  /* Send the message itself */
  nbytes = write(pipe_to_proxy, buf, buf_len);

  if(nbytes != buf_len){

    fprintf(stderr, "Send_proxy_message: error writing to pipe\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Get_proxy_message(int pipe_from_proxy, char *buf, int *nbytes)
{
  int  i;
  int *int_ptr;
  int  num_recvd;
  char rubbish;

  /* Routine assumes buf points to a large enough buffer to receive
     message */
  if(!buf){

    fprintf(stderr, "Get_proxy_message: NULL ptr to data buffer\n");
    return REG_FAILURE;
  }

  /* Block until message received */
  while( (*nbytes = read(pipe_from_proxy, buf, HEADER_BYTES)) == 0){

    sleep(1);
    fprintf(stderr, ".");
  }
 
  if(*nbytes == -1){

    fprintf(stderr, "Get_proxy_message: error reading from pipe\n");
    return REG_FAILURE;
  }

  int_ptr = (int *)buf;
  *nbytes = *int_ptr;

  fprintf(stderr, "Get_proxy_message: Read nbytes = %d\n", *nbytes);

  if(*nbytes > MAX_MSG_SIZE){

    fprintf(stderr, "Get_proxy_message: WARNING: truncating message\n");
 
    read(pipe_from_proxy, buf, MAX_MSG_SIZE);

    /* Throw-away rest of message */
    for(i=0; i<(*nbytes - MAX_MSG_SIZE); i++){

      read(pipe_from_proxy, &rubbish, 1);
    }
  }
  else{

    fprintf(stderr, "Get_proxy_message: reading %d bytes...\n", *nbytes);

    if( (num_recvd = read(pipe_from_proxy, buf, *nbytes)) != *nbytes ){

      fprintf(stderr, "Get_proxy_message: failed to read message\n");
      *nbytes = num_recvd;
      return REG_FAILURE;
    }
  }

#if DEBUG
  fprintf(stderr, "Get_proxy_message: received: %s\n", buf);
#endif

  return REG_SUCCESS;
}
