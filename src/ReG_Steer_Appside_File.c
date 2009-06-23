/*----------------------------------------------------------------------------
  This file contains routines and data structures for file-based
  steering communication and sample transfer.

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

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Appside_File.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Files_Common.h"

/** @internal
    @file ReG_Steer_Appside_File.c
    @brief Source file for file-related Appside routines
    @author Andrew Porter
    @author Robert Haines
  */

/** @internal
   The table holding details of our communication channel with the
   steering client - defined in ReG_Steer_Appside.c */
extern Steerer_connection_table_type Steerer_connection;

/** @internal
   The table holding details of the registered IOTypes for this
   application - defined in ReG_Steer_Appside.c */
extern IOdef_table_type IOTypes_table;

/** @internal Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*-----------------------------------------------------------------------*/

/* int Detach_from_steerer_file(){ */

/*   int   nbytes; */
/*   char  filename[REG_MAX_STRING_LENGTH]; */

/*   /\* Remove lock file that indicates app is being steered *\/ */

/*   nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",  */
/* 		    Steerer_connection.file_root,  */
/* 		    STR_CONNECTED_FILENAME); */

/*   if(nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){ */

/*     fprintf(stderr, "STEER: Detach_from_steerer_file: name of lock-file exceeds %d" */
/* 	    " characters - increase REG_MAX_STRING_LENGTH\n",  */
/* 	    REG_MAX_STRING_LENGTH); */
/*     return REG_FAILURE; */
/*   } */

/*   remove(filename); */

/*   /\* Remove any files that steerer has produced that we won't */
/*      now be consuming *\/ */

/*   nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",  */
/* 		    Steerer_connection.file_root,  */
/* 		    STR_TO_APP_FILENAME); */

/*   if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){ */

/*     fprintf(stderr, "STEER: Detach_from_steerer_file: name of steerer ctrl files " */
/* 	    "exceeds %d characters - increase REG_MAX_STRING_LENGTH\n",  */
/* 	    REG_MAX_STRING_LENGTH); */
/*     return REG_FAILURE; */
/*   } */

/*   Remove_files(filename); */
/*   return REG_SUCCESS; */
/* } */

/* /\*--------------------------------------------------------------------*\/ */

/* int Steerer_connected_file() */
/* { */
/*   char   filename[REG_MAX_STRING_LENGTH]; */
/*   FILE  *fp; */
/*   int    nbytes; */

/*   nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",  */
/* 		    Steerer_connection.file_root,  */
/* 		    STR_CONNECTED_FILENAME); */

/*   if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){ */

/*     fprintf(stderr, "STEER: Steerer_connected_file: full path name of lockfile " */
/* 	    "indicating steerrer connected exceeds %d chars - increase " */
/* 	    "REG_MAX_STRING_LENGTH\n", REG_MAX_STRING_LENGTH); */
/*     return REG_FAILURE; */
/*   } */

/*   if( (fp = fopen(filename, "r")) ){ */

/*       fclose(fp); */
/*       return REG_SUCCESS; */
/*   } */

/*   return REG_FAILURE; */
/* } */

/*----------------------------------------------------------------*/

/* int Generate_status_filename(char* filename) */
/* { */

/* #ifdef UNICORE_DEMO */

/*   /\* Always just output <path>/steer_status for UNICORE demo *\/ */
/*   sprintf(filename, "%ssteer_status", Steerer_connection.file_root); */

/* #else /\* Not UNICORE demo - use full, indexed filenames *\/ */

/*   static int output_file_index = 0; */

/*   /\* Generate next filename in sequence for sending data to */
/*      steerer & increment counter *\/ */

/*   sprintf(filename, "%s%s_%d", Steerer_connection.file_root,  */
/* 	  APP_TO_STR_FILENAME, output_file_index++); */

/*   /\* Wrap counter if no. of distinct files exceeded *\/ */

/*   if(output_file_index == REG_MAX_NUM_FILES) output_file_index = 0; */

/* #endif /\* UNICORE_DEMO *\/ */

/*   return REG_SUCCESS; */
/* } */

/*-------------------------------------------------------------------*/

/* int Send_status_msg_file(char *buf) */
/* { */
/*   FILE *fp; */
/*   char  filename[REG_MAX_STRING_LENGTH]; */

/*   Generate_status_filename(filename); */

/*   if( (fp = fopen(filename, "w")) == NULL){ */

/*     fprintf(stderr, "STEER: Send_status_msg: failed to open file <%s>\n", */
/* 	    filename); */
/*     return REG_FAILURE; */
/*   } */

/*   fprintf(fp, "%s", buf); */
/*   fclose(fp); */

/*   Create_lock_file(filename); */

/*   return REG_SUCCESS; */
/* } */

/*-------------------------------------------------------------------*/

/* struct msg_struct *Get_control_msg_file() */
/* { */
/*   struct msg_struct   *msg = NULL; */
/*   FILE                *fp; */
/*   char                 filename[REG_MAX_STRING_LENGTH]; */
/*   int                  nbytes; */

/*   nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s",  */
/* 		    Steerer_connection.file_root,  */
/* 		    STR_TO_APP_FILENAME); */

/*   if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){ */

/*     fprintf(stderr, "STEER: Get_control_msg_file: length of ctrl msg filename " */
/* 	    "exceeds %d chars - increase REG_MAX_STRING_LENGTH\n",  */
/* 	    REG_MAX_STRING_LENGTH); */
/*   } */

/*   if( (fp = Open_next_file(filename)) != NULL){ */

/*     fclose(fp); */

/*     msg = New_msg_struct(); */

/*     /\* Pass NULL down here as this is app-side and we have no ptr to */
/*        a Sim_entry struct *\/ */
/*     if(Parse_xml_file(filename, msg, NULL) != REG_SUCCESS){ */

/*       fprintf(stderr, "STEER: Get_control_msg_file: failed to parse <%s>\n", filename); */
/*       Delete_msg_struct(&msg); */
/*     } */

/*     /\* Delete the file once we've read it *\/ */
/*     if( Delete_file(filename) != REG_SUCCESS){ */

/*       fprintf(stderr, "STEER: Get_control_msg_file: failed to delete %s\n",filename); */
/*     } */
/*   } */

/*   return msg; */
/* } */

/*-------------------------------------------------------------------*/

/* int Finalize_steering_connection_file() */
/* { */
/*   char sys_command[REG_MAX_STRING_LENGTH]; */

/* #ifdef REG_DEBUG */
/*   int  max, max1; */

/*   max = strlen(APP_STEERABLE_FILENAME); */
/*   max1 = strlen(STR_CONNECTED_FILENAME); */

/*   if(max1 > max) max=max1; */
  
/*   max += strlen(Steerer_connection.file_root); */
/*   if(max > REG_MAX_STRING_LENGTH ){ */

/*     fprintf(stderr, "STEER: Finalize_steering_connection_file: WARNING: truncating " */
/* 	    "filename\n"); */
/*   } */
/* #endif */

/*   /\* Delete the lock file that indicates we are steerable *\/ */
/*   sprintf(sys_command, "%s%s", Steerer_connection.file_root, */
/* 	  APP_STEERABLE_FILENAME); */
/*   if(remove(sys_command)){ */

/*     fprintf(stderr, "STEER: Finalize_steering_connection_file: failed to remove " */
/* 	    "%s\n", sys_command); */
/*   } */

/*   /\* Delete the lock file that indicates we are being steered *\/ */
/*   sprintf(sys_command, "%s%s", Steerer_connection.file_root, */
/* 	  STR_CONNECTED_FILENAME); */
/*   if(remove(sys_command)){ */
/* #ifdef REG_DEBUG     */
/*     fprintf(stderr, "STEER: Finalize_steering_connection_file: failed to remove " */
/* 	    "%s\n", sys_command); */
/* #endif */
/*   } */

/*   /\* Delete any files we'd have consumed if we'd lived longer *\/ */
/*   sprintf(sys_command, "%s%s", Steerer_connection.file_root,  */
/* 	  STR_TO_APP_FILENAME); */
/*   Remove_files(sys_command); */

/*   return REG_SUCCESS; */
/* } */

/*-------------------------------------------------------------------*/

/* int Initialize_steering_connection_file(int  NumSupportedCmds, */
/* 					int *SupportedCmds) */
/* { */
/*   FILE *fp; */
/*   char  buf[REG_MAX_MSG_SIZE]; */
/*   char  filename[REG_MAX_STRING_LENGTH]; */

/*   /\* Set location of all comms files *\/ */
/*   if(Set_steering_directory() != REG_SUCCESS){ */

/*     return REG_FAILURE; */
/*   } */

/*   /\* Clean up any old files... *\/ */

/*   /\* ...file indicating a steerer is connected (which it can't be since we've */
/*      only just begun) *\/  */
/*   sprintf(filename, "%s%s", Steerer_connection.file_root,  */
/* 	  STR_CONNECTED_FILENAME); */
/*   fp = fopen(filename, "w"); */
/*   if(fp != NULL){ */

/*     fclose(fp); */
/*     if(remove(filename)){ */

/*       fprintf(stderr, "STEER: Initialize_steering_connection_file: failed to " */
/* 	      "remove %s\n",filename); */
/*     } */
/* #ifdef REG_DEBUG */
/*     else{ */
/*       fprintf(stderr, "STEER: Initialize_steering_connection_file: removed " */
/* 	      "%s\n", filename); */
/*     } */
/* #endif */
/*   } */

/*   /\* ...files containing messages from a steerer *\/ */
/*   sprintf(filename, "%s%s", Steerer_connection.file_root,  */
/* 	  STR_TO_APP_FILENAME); */

/*   Remove_files(filename); */

/*   /\* Signal that component is available to be steered *\/ */

/*   sprintf(filename, "%s%s", Steerer_connection.file_root,  */
/* 	                    APP_STEERABLE_FILENAME); */
/*   fp = fopen(filename,"w"); */

/*   if(fp == NULL){ */

/*     fprintf(stderr, "STEER: Initialize_steering_connection_file: failed to open %s\n", */
/* 	    filename); */
/*     return REG_FAILURE; */
/*   } */

/* #ifdef REG_DEBUG */
/*   fprintf(stderr, "STEER: Initialize_steering_connection_file: writing file: %s\n",  */
/* 	  filename); */
/* #endif */

/*   Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, buf, REG_MAX_MSG_SIZE); */

/*   fprintf(fp, "%s", buf); */
/*   fclose(fp); */

/*   return REG_SUCCESS; */
/* } */

/*-----------------------------------------------------------------------*/

/* int Get_data_source_address_file(int                 dummy, */
/* 				 char               *hostname, */
/* 				 unsigned short int *port) */
/* { */
/*   char *pchar; */
/*   int   len; */

/*   /\* Return port = 0 on failure *\/ */
/*   *port = 0; */

/*   /\* Get hostname and port from environment variables *\/ */

/*   pchar = getenv("REG_CONNECTOR_HOSTNAME"); */
/*   if (pchar) { */
/*     len = strlen(pchar); */
/*     if (len < REG_MAX_STRING_LENGTH) { */
/*       sprintf(hostname, pchar); */
/*     } */
/*     else{ */
/*       fprintf(stderr, "STEER: Get_data_source_address_file: content of " */
/* 	      "REG_CONNECTOR_HOSTNAME exceeds max. string length of " */
/* 	      "%d chars\n", REG_MAX_STRING_LENGTH); */
/*       return REG_FAILURE; */
/*     } */
/*   } */
/*   else{ */
/*     fprintf(stderr,  */
/* 	    "STEER: Get_data_source_address_file: REG_CONNECTOR_HOSTNAME not set\n"); */
/*     return REG_FAILURE; */
/*   } */

/*   pchar = getenv("REG_CONNECTOR_PORT"); */
/*   if (pchar) { */
/*     *port =  (unsigned short int)atoi(pchar); */
/*   } */
/*   else{ */
/*     fprintf(stderr,  */
/* 	    "STEER: Get_data_source_address_file: REG_CONNECTOR_PORT not set\n"); */
/*     return REG_FAILURE; */
/*   } */

/*   return REG_SUCCESS; */
/* } */

/*-----------------------------------------------------------------------*/
