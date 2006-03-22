/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  file-based data communication.

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
#ifndef __REG_STEER_APPSIDE_FILE_H__
#define __REG_STEER_APPSIDE_FILE_H__

/** @internal
    @file ReG_Steer_Appside_File.h
    @brief Header file for file-related Appside routines
    @author Andrew Porter
    @author Robert Haines
  */

/** @internal
    Removes files, signalling that client is attached and deletes
    any remaining files containing messages sent by client */
int Detach_from_steerer_file();

/** @internal
    @return REG_SUCCESS if client is attempting to connect

    Check for a connection from a steering client. */
int Steerer_connected_file();

/** @internal
    @param filename Root of filename for status message

    Generate a filename (for a status message) from supplied root and 
    internally-generated index */
int Generate_status_filename(char* filename);

/** @internal
    @param buf The status message to send

    Send the contents of @p buf to the steering client by writing to 
    an appropriate file. */
int Send_status_msg_file(char *buf);

/** @internal
    Check for, and read, the next available message from an attached
    steering client. */
struct msg_struct *Get_control_msg_file();

/** @internal
    @param NumSupportedCmds No. of commands support by application
    @param SupportedCmds Array containing the supported commands

    Set-up and advertise application as steerable - write a lock
    file in the REG_STEER_DIRECTORY directory. */
int Initialize_steering_connection_file(int  NumSupportedCmds,
					int *SupportedCmds);

/** @internal Take down any connection to a steering client */
int Finalize_steering_connection_file();

/** @internal
    @param dummy Not used because we only support a single input
    channel in file-based steering
    @param hostname hostname of machine on which data source is listening
    @param port Port on which data source is listening

    Get the data source for the dummy'th input channel. Since we
    only use REG_CONNECTOR_PORT/HOSTNAME for this we only have
    info. for a single input channel - hence 'dummy' */
int Get_data_source_address_file(int                 dummy,
				 char               *hostname,
				 unsigned short int *port);

/** @internal
    @param direction Type of IOType, REG_IO_IN or REG_IO_OUT.
    @param index Index of the IOType to initialize

    Initialise IOType-related stuff for file-based IO.  Currently
    just gets the directory in which to put data files (or read 
    them from) from the REG_DATA_DIRECTORY env. variable */ 
int Initialize_IOType_transport_file(int direction, 
				     int index);

/** @internal
    @param index Index of the IOType to check

    Checks to see if there is any data available to read from 
    an IOtype */
int Consume_start_data_check_file(int index);

/** @internal
    @param fileroot Pattern to pass to 'ls' command
    @param num Number of files found
    @param names Array of char* holding names of files found

    Searches for files matching the pattern specified in fileroot (@e
    i.e.  does 'ls <fileroot>').  If any are found, @p names is
    malloc'd to point to an array of char* and each entry in this
    array is malloc'd and set to the relevant filename. These ptrs
    MUST be free'd. @p num can be zero even if the routine returns
    REG_SUCCESS. */
int Get_file_list(char *fileroot,
		  int  *num,
		  char ***names);

/** @internal
    @param index Index of IOType from which to read data.
    @param datatype Type of data to read
    @param num_bytes_to_read No. of bytes to read
    @param pData Pointer to buffer in which to store non-xdr-encoded
    data

    Reads specified number of bytes from the file associated with the
    IOType.  @p pData must point to a large enough buffer unless xdr
    is being used (default) in which case xdr-encoded data is stored
    in internal IOType buffer. */
int Consume_data_read_file(const int	 index,  
			   const int	 datatype,
			   const size_t	 num_bytes_to_read, 
			   void		*pData);

/** @internal
    @param index Index of IOType to emit data on
    @param num_bytes_to_send No. of bytes of data to emit
    @param pData Pointer to buffer containing data to emit

    Writes the specified no. of bytes to the file associated with
    the IOType with index @p index. */
int Emit_data_file(const int	 index,
		   const size_t	 num_bytes_to_send,
		   void		*pData);

/** @internal
    @param index Index of IOType from which to read data.
    @param DataType Type of data in next data slice
    @param Count No. of data elements in next data slice
    @param NumBytes No. of bytes of data in next data slice
    @param IsFortranArray Whether next data slice is data from
    a Fortran array (affects its ordering)

    Reads the header of an incoming data slice from the file
    associated with the specified IOType. */
int Consume_msg_header_file(int  index,
			    int *DataType,
			    int *Count,
			    int *NumBytes,
			    int *IsFortranArray);

/** @internal
    @param index Index of IOType to check for acknowledgement on

    Check for acknowledgement that consumer has read the last data set
    that we emitted */
int Consume_ack_file(const int index);

/** @internal
    @param index Index of IOType to emit acknowledgement on

    Create an acknowledgement that we've consumed the last data set
    sent to us */
int Emit_ack_file(const int index);

#endif
