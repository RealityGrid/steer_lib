/*----------------------------------------------------------------------------
  (C) Copyright 2006, University of Manchester, United Kingdom,
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
---------------------------------------------------------------------------*/

/** @internal
    @file ReG_Steer_Proxy_utils.c
    @brief Routines and structures for communicating with java proxy process

    Library routines and associated data structures for use in
    creating and communicating with a steering-proxy process (as
    defined in SteeringProxy.java).
    @author Andrew Porter
    @author Robert Haines
*/
#include "ReG_Steer_Config.h"
#include "ReG_Steer_Proxy_utils.h"

/*
#ifndef REG_DEBUG
#define REG_DEBUG 1
#endif
*/

#ifndef WIN32
#else
#include <io.h>
/*
NOW IN CMAKE

#define read _read
*/
#endif

/*----------------------------------------------------------*/

/* NOTE - This function is not currently available under Windows */
int Create_proxy(int *to_proxy, int *from_proxy)
{
#ifndef WIN32
  const int stdin_fd  = 0;
  const int stdout_fd = 1;

  int   status;
  pid_t my_id;

  int   pipe_to_proxy[2];
  int   pipe_from_proxy[2];

  int   nbytes;
  char  msg[REG_MAX_MSG_SIZE];

  char *registry_gsh;
  char  registry_arg[REG_MAX_STRING_LENGTH];
  char *exec_path = "ReG_Steer_Proxy";
  char *class_path;

  /* Get the value of the CLASSPATH environment variable for launching
     the java proxy */
  if( (class_path = getenv("CLASSPATH")) == NULL){

    fprintf(stderr, "STEER: Create_proxy: failed to get CLASSPATH env. variable\n");
    return REG_FAILURE;
  }

  /* Check that the proxy we're going to launch can be found in the 
     classpath - otherwise we get stuck after the execlp */
  if( Proxy_is_in_path(class_path, exec_path) != REG_SUCCESS ){

    fprintf(stderr, "STEER: Create_proxy: proxy is not on specified CLASSPATH\n");
    return REG_FAILURE;
  }

  /* Get the value of the REGISTRY_GSH environment variable - this gives
     the grid service handle of the registry that the proxy is to
     bind to */
  if( (registry_gsh = getenv("REGISTRY_GSH")) == NULL){

    fprintf(stderr, "STEER: Create_proxy: failed to get REGISTRY_GSH env. "
	            "variable\n");
    return REG_FAILURE;
  }

  sprintf(registry_arg, "-Dregistry.gsh=%s", registry_gsh);

  /* Create pipe for sending messages _to_ proxy */
  if(pipe(pipe_to_proxy)){

    fprintf(stderr, "STEER: Create_proxy: failed to create pipe_to_proxy...\n");
    return REG_FAILURE;
  }

  *to_proxy = pipe_to_proxy[1];

  /* Create pipe for receiving messages _from_ proxy */
  if(pipe(pipe_from_proxy)){

    fprintf(stderr, "STEER: Create_proxy: failed to create pipe_from_proxy...\n");
    return REG_FAILURE;
  }

  *from_proxy = pipe_from_proxy[0];

  /* Launch new child thread */
  my_id = fork();

  if(my_id == -1){

    fprintf(stderr, "STEER: Create_proxy: call to fork() failed...\n");
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

      fprintf(stderr, "STEER: Failed to attach to stdin, status = %d\n", status);
      Send_proxy_message(pipe_from_proxy[1], ERR_MSG);
      exit(1);
    }

    /* Close the Child process' STDOUT */
    close(stdout_fd);

    /* Duplicate the Child's STDOUT to the pipe_from_proxy[1] file 
       descriptor */
    if( (status = dup(pipe_from_proxy[1])) != stdout_fd){

      fprintf(stderr, "STEER: Failed to attach to stdout, status = %d\n", status);
      Send_proxy_message(pipe_from_proxy[1], ERR_MSG);
      exit(1);
    }

    /*  Make the exec call to run the java program. */
    execlp("java","java", "-Xmx20m", registry_arg, 
	   "-classpath", class_path, exec_path, (char *)0);

    /* If execlp returned then it failed... */
    fprintf(stderr, "STEER: Create_proxy: execv failed...\n");

    Send_proxy_message(pipe_from_proxy[1], ERR_MSG);
    exit(1);
  }
  else{
    /* Am parent process */

    /* Check that proxy has launched OK */
    Get_proxy_message(pipe_from_proxy[0], msg, &nbytes);

    if(strncmp(msg, ERR_MSG, nbytes) == 0){

      return REG_FAILURE;
    }
  }

#endif
  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Destroy_proxy(int pipe_to_proxy)
{
  return Send_proxy_message(pipe_to_proxy, "QUIT");
}

/*----------------------------------------------------------*/

/* NOTE - This function is not currently available under Windows */
int Send_proxy_message(int pipe_to_proxy, const char *buf)
{
#ifndef WIN32
  int   nbytes;
  int   buf_len;
  int   foot_len;
  int   tot_len;
  char *complete_msg;
  char  footer[REG_MAX_STRING_LENGTH];

  buf_len = strlen(buf);

  /* Messages are done in terms of lines so make sure we end in '\n'
     (END_OF_MSG includes a '\n') */
  if(buf[buf_len-1] != '\n'){

    sprintf(footer, "\n%s", END_OF_MSG);
  }
  else{

    strcpy(footer, END_OF_MSG);
  }
    
  foot_len = strlen(footer);
  tot_len  = buf_len + foot_len ;

  complete_msg = (char *)malloc(tot_len+1);

  strcpy(complete_msg, buf);
  strcpy(&(complete_msg[buf_len]), footer);

  nbytes = write(pipe_to_proxy, complete_msg, tot_len);

  free(complete_msg);

  if(nbytes != tot_len){

    fprintf(stderr, "STEER: Send_proxy_message: error writing to pipe\n");
    return REG_FAILURE;
  }

  /*
  * Send how many bytes... *
  buf_len = strlen(buf);
  len = (char *) &buf_len;

  nbytes = write(pipe_to_proxy, len, REG_HEADER_BYTES);

  if(nbytes != REG_HEADER_BYTES){

    fprintf(stderr, "STEER: Send_proxy_message: error writing to pipe\n");
    return REG_FAILURE;
  }

  * Send the message itself *
  nbytes = write(pipe_to_proxy, buf, buf_len);

  if(nbytes != buf_len){

    fprintf(stderr, "STEER: Send_proxy_message: error writing to pipe\n");
    return REG_FAILURE;
  }
  */
#endif
  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

/* NOTE - This function is not currently available under Windows */
int Get_proxy_message(int pipe_from_proxy, char *buf, int *nbytes)
{
#ifndef WIN32
  char *pbuf;
  char  line_buf[REG_MAX_LINE_LEN];
  int   count;
  int   len;

  /* Routine assumes buf points to a large enough buffer to receive
     message */
  if(!buf){

    fprintf(stderr, "STEER: Get_proxy_message: NULL ptr to data buffer\n");
    return REG_FAILURE;
  }

  /* Wipe buffer to avoid confusion */
  memset(buf, 0, REG_MAX_MSG_SIZE);

  /* Block until message received */
#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Get_proxy_message: waiting for msg from proxy...\n");
#endif

  while( (len = getline(line_buf, REG_MAX_LINE_LEN, pipe_from_proxy)) == 0){

    sleep(1);
    fprintf(stderr, ".");
  }
 
  if(len == -1){

    fprintf(stderr, "STEER: Get_proxy_message: error reading from pipe\n");
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Get_proxy_message: got: <%s>\n", line_buf);
  fprintf(stderr, "STEER: Get_proxy_message: len = %d\n", len);
#endif

  pbuf  = buf;
  count = 0;

  while(strncmp(line_buf, END_OF_MSG, strlen(END_OF_MSG))){


    if((count+len) < REG_MAX_MSG_SIZE){

      count += len;
      strcpy(pbuf, line_buf);

      pbuf += len;
    }

    /* Get the next line - exit if EOF or error */
    len = getline(line_buf, REG_MAX_LINE_LEN, pipe_from_proxy);

#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Get_proxy_message: got: <%s>\n", line_buf);
    fprintf(stderr, "STEER: Get_proxy_message: len = %d\n", len);
#endif

    if(len == 0){

      fprintf(stderr, "STEER: Get_proxy_message: hit EOD while reading message\n");
      return REG_FAILURE;
    }
    else if(len == -1){

      fprintf(stderr, "STEER: Get_proxy_message: hit error while reading message\n");
      return REG_FAILURE;
    }
  }

  if(count >= REG_MAX_MSG_SIZE){

    fprintf(stderr, "STEER: Get_proxy_message: WARNING: truncating message\n");
    return REG_FAILURE;
  }

  *nbytes = count;

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Get_proxy_message: received: %s\n", buf);
#endif

#endif
  return REG_SUCCESS;
}

/*-------------------------------------------------------------*/

/** @internal
    @brief Routine taken from Kernighan and Ritchie, slightly tweaked */

int getline(char s[], int lim, int fd)
{
  int n=0;
  int i=0;
  char c;

  while(--lim > 0 && (n=read(fd, &c, 1)) == 1 && c != '\n')
    s[i++] = c;

  if(n == -1)
     return -1;

  if(c == '\n')
    s[i++] = c;

  s[i] = '\0';

  return i;
}

/*-------------------------------------------------------------*/

int Proxy_is_in_path(const char *class_path, const char *exec)
{
  char       *path;
  char       *tmp_path;
  char        full_path[256];
  int         return_status;
  struct stat stbuf;

  return_status = REG_FAILURE;

  /* Take a copy of the class path passed to us because what we're 
     about to do will trash it */
  tmp_path = (char *)malloc((strlen(class_path)+1)*sizeof(char));
  strcpy(tmp_path, class_path);

  path = strtok(tmp_path, ":");

  while(path){

    /* Construct full path to the named class */
    sprintf(full_path, "%s/%s.class", path, exec); 

    if(stat(full_path, &stbuf) != -1){

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Proxy_is_in_path: found >%s<\n", full_path);
#endif
      return_status = REG_SUCCESS;
      break;
    }
    path = strtok(NULL, ":");
  }

  free(tmp_path);

  return return_status;
}
