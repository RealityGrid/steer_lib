/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  SOAP-based steering communication.

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
#ifndef __REG_STEER_APPSIDE_FILE_H__
#define __REG_STEER_APPSIDE_FILE_H__

int Detach_from_steerer_file();

/* Check for a connection from a steering client - return REG_SUCCESS
   if a client is attempting to connect */
int Steerer_connected_file();

/* Generate a filename (for a status message) from supplied root and 
   internally-generated index */
int Generate_status_filename(char* filename);

/* Send the contents of *buf to the steering client by writing to 
   an appropriate file. */
int Send_status_msg_file(char *buf);

/* Check for, and read, the next available message from an attached
   steering client. */
struct msg_struct *Get_control_msg_file();

/* Set-up and advertise application as steerable - write a lock
   file in the REG_STEER_DIRECTORY directory. */
int Initialize_steering_connection_file(int  NumSupportedCmds,
					int *SupportedCmds);

/* Take down any connection to a steering client */
int Finalize_steering_connection_file();

/* Get the data source for the dummy'th input channel. Since we
   only use REG_CONNECTOR_PORT/HOSTNAME for this we only have
   info. for a single input channel - hence 'dummy' */
int Get_data_source_address_file(int                 dummy,
				 char               *hostname,
				 unsigned short int *port);

/* Initialise IOType-related stuff for file-based IO.  Currently
   just gets the directory in which to put data files (or read 
   them from) from the REG_DATA_DIRECTORY env. variable */ 
int Initialize_IOType_transport_file(int direction, 
				     int index);

/* Checks to see if there is any data available to read for IOtype
   with index 'index' */
int Consume_start_data_check_file(int index);

/* Searches for files matching the pattern specified in fileroot (i.e.
   does 'ls <fileroot>').
   If any are found, names is malloc'd to point to an array of char*
   and each entry in this array is malloc'd and set to the relevant
   filename. These ptrs MUST be free'd. *num can be zero even if the
   routine returns REG_SUCCESS. */
int Get_file_list(char *fileroot,
		  int  *num,
		  char ***names);

int Consume_data_read_file(const int	 index,  
			   const int	 datatype,
			   const size_t	 num_bytes_to_read, 
			   void		*pData);

int Emit_data_file(const int	 index,
		   const size_t	 num_bytes_to_send,
		   void		*pData);

int Consume_msg_header_file(int  index,
			    int *DataType,
			    int *Count,
			    int *NumBytes,
			    int *IsFortranArray);
#endif
