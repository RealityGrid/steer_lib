/*----------------------------------------------------------------------------
    This header file contains routines and data structures for
    SOAP-based steering communication.

    (C)Copyright 2003, The University of Manchester, United Kingdom,
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

    Initial version by:  A Porter, 23.4.2003       0.1

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


int Send_status_msg_file(char *buf);

struct msg_struct *Get_control_msg_file();

int Initialize_steering_connection_file(int  NumSupportedCmds,
					int *SupportedCmds);

/* Take down any connection to a steering client */
int Finalize_steering_connection_file();

int Get_data_source_address_file(int   dummy,
				 char *hostname,
				 int  *port);

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
