/*----------------------------------------------------------------------------
  Header file containing prototypes and datatype definitions for
  entities that are common to both the application- and steerer-side
  of the RealityGrid steering library.

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

#ifndef __REG_STEER_COMMON_H__
#define __REG_STEER_COMMON_H__

#include "ReG_Steer_Appside_Sockets.h"

/* Following two includes are for use of stat system call 
   in Open_next_file */
#include <sys/types.h>
#include <sys/stat.h>

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

/* Structures for checkpoint logging */

typedef struct {

  int  handle;
  char value[REG_MAX_STRING_LENGTH];

} Param_log_entry_type;

typedef struct {

  int   id;
  char  params[REG_MAX_STRING_LENGTH];

} Cmd_log_entry_type;

typedef struct {

  /* For parameter value changes */
  int   num_params;
  Param_log_entry_type param[REG_MAX_NUM_STR_PARAMS];
  /* For other cmds */
  int   num_cmds;
  Cmd_log_entry_type cmd[REG_MAX_NUM_STR_CMDS];

} Steer_log_type;

typedef struct {

  int                  key;
  int                  chk_handle;
  char                 chk_tag[REG_MAX_STRING_LENGTH];
  int                  num_param;
  Param_log_entry_type param[REG_MAX_NUM_STR_PARAMS];
  int                  sent_to_steerer;

} Chk_log_entry_type;

typedef enum {PARAM, CHKPT} log_type_type ;

typedef struct {

  /* What sort of log this is */
  log_type_type       log_type;
  /* Current no. of entries in table */
  int                 num_entries;
  /* Max. no. of entries table can currently hold (is dynamic) */
  int                 max_entries;
  /* No. of entries we have that haven't been sent to the steerer */
  int                 num_unsent;
  /* Next primary-key value */
  int                 primary_key_value;
  /* Array of entries */
  Chk_log_entry_type *entry;
  /* Ptr to file containing previous log entries */
  FILE               *file_ptr;
  /* Name of this file */
  char                filename[REG_MAX_STRING_LENGTH];
  /* Flag to indicate whether or not all entries must be sent to
     steerer, IRRESPECTIVE of the value of their individual 
     sent_to_steerer flags.  This used when a client detaches and
     another one attaches some time later. */
  int                 send_all;
  int                 param_send_all[REG_MAX_NUM_STR_PARAMS];
  /* Flag to indicate whether a send of the log data (read in on
     a previous occasion) is still in progress.  This feature
     prevents a deluge of log messages being emitted. */
  int                 emit_in_progress;
  /* Count of how many log messages we've sent in this current call
     of Steering_control. If this reaches REG_MAX_NUM_LOG_MSG then
     we set the emit_in_progress flag and continue next time. */
  int                 num_sent;
  /* Ptr to buffer holding contents of log buffer for log
     emits spread over several calls to Emit_log */
  char               *file_content;
  /* Ptr to buffer containing logged steering cmds */
  char               *pSteer_cmds;
  /* Ptr to next free space in this buffer */
  char               *pSteer_cmds_slot;
  /* No. of bytes currently allocated to buffer */
  int                 steer_cmds_bytes;

} Chk_log_type;


/* Types and structures for reading parameter defs */

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   steerable;
  int   type;
  int   handle;
  void *ptr;
  void *ptr_raw;
  char  value[REG_MAX_STRING_LENGTH];
  int   modified;
  int   is_internal;
  int   min_val_valid;
  char  min_val[REG_MAX_STRING_LENGTH];
  int   max_val_valid;
  char  max_val[REG_MAX_STRING_LENGTH];

  double* log;
  int     log_index;
  int     log_size;

} param_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  param_entry *param;

} Param_table_type;

/* Types and structures for reading supported commands */

typedef struct {

  int               cmd_id;
  Param_table_type  cmd_params;

}supp_cmd_entry;

typedef struct {

  int             num_registered;
  int             max_entries;
  supp_cmd_entry *cmd;

} Supp_cmd_table_type;

/* Types and structures for reading IO types */

typedef struct {

  /* Whether array data is in F90 ordering */
  int is_f90;
  /* Extent of whole array */
  int totx, toty, totz;
  /* Extent of sub-array */
  int nx, ny, nz;
  /* Origin of sub-array within whole */
  int sx, sy, sz;

} Array_type;

typedef struct {
  /* Label of IOType (supplied by user) */
  char				label[REG_MAX_STRING_LENGTH];
  /* Handle of IOType - generated by us & returned to user */
  int				handle;

  /* For file-based IO */
  char				filename[REG_MAX_STRING_LENGTH];
  char                          directory[REG_MAX_STRING_LENGTH];
  FILE                         *fp;

  /* Whether input, output */
  int				direction;
  /* Variable holding the frequency with which emission/consumption is
     to occur (every frequency steps) */
  int                           frequency;
  /* Handle of the (steerable) frequency in the parameter table */
  int				freq_param_handle;

  /* Pointer to buffer to hold data */
  void			       *buffer;
  /* How much data there is currently in the buffer */
  int                           buffer_bytes;
  /* Size of this buffer */
  int				buffer_max_bytes;
#if REG_SOCKET_SAMPLES
  /* structure used to hold all socket information */
  socket_io_type		socket_info;
#endif
  /* Whether or not to encode non-ASCII data as XDR (set in Emit_start) */
  int				use_xdr;
  /* How many bytes of xdr data to read (to avoid having to work-out/guess
     how many it will be from the type) */
  int                           num_xdr_bytes;
  /* Details on incoming array (if available) */
  Array_type                    array;
  /* Whether or not (1 or 0) we'll need to convert the ordering of 
     the array */
  int                           convert_array_order;
  /* Whether IOType is enabled or not (for sockets - whether socket
     has been created) */
  int                           is_enabled;
  /* Index of the input channel - used in mapping to the details
     held by the SGS (when steering via SOAP) */
  int                           input_index;
  /* Whether or not this IOType (direction OUT) cares about getting 
     an acknowledgement before trying to emit the next sample */
  int                           use_ack;
  /* Whether or not we need to check for an acknowledgement before
     attempting to emit the next data set.  'use_ack' OVERRIDES
     this flag. */
  int                           ack_needed;

} IOdef_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  int          enable_on_registration; /* Whether or not to create the socket
					  (if using them) when an IOType
					  is registered */
  int          num_inputs; /* Count of input channels registered - used to
			      map to details on data sources held by SGS */
  IOdef_entry *io_def;

}IOdef_table_type;

/*-------- Function prototypes --------*/

extern PREFIX FILE *Open_next_file(char* base_name);

extern PREFIX int Create_lock_file(char *filename);

extern PREFIX int Delete_file(char *filename);

extern PREFIX int Directory_valid(char *directory);

/* Routine to get (& check validity of) steering scratch directory
   from REG_STEER_DIRECTORY env. variable.  Used even when steering
   is NOT file based. */
extern PREFIX int Set_steering_directory();

extern PREFIX int Next_free_param_index(Param_table_type *table);

/* A look-up function - return the index of the parameter with handle
   ParamHandle in the table pointed to by *table. Returns 
   REG_PARAM_HANDLE_NOTSET if no matching handle found. */
extern PREFIX int Param_index_from_handle(Param_table_type *table, 
				   int ParamHandle);

/* A look-up function - return the index of the IOdef with handle 
   IOdefHandle in the table pointed to by *table.  Returns
   REG_IODEF_HANDLE_NOTSET if no matching handle found. */
extern PREFIX int IOdef_index_from_handle(IOdef_table_type *table, 
					  int IOdefHandle);

extern PREFIX int Increment_param_registered(Param_table_type *table);

extern PREFIX int Increment_cmd_registered(Supp_cmd_table_type *table);

extern PREFIX int Increment_iodef_registered(IOdef_table_type *table);

extern PREFIX int Increment_log_entry(Chk_log_type *log);

/* Called when steering finished - cleans up any files that either the app
   or steerer hasn't got around to consuming */

extern PREFIX int Remove_files(char* base_name);

extern PREFIX int Get_message_type(const char *name);

/* Write ReG-specific XML header & footer information into supplied message
   buffer */
extern PREFIX int Write_xml_header(char **pchar);

extern PREFIX int Write_xml_footer(char **pchar,
				   int bytes_free);

/* Read the specified ASCII file and return the contents in the buffer
   pointed to by *buf, the size of which is returned in *size.  It is
   the caller's responsibility to free() the memory pointed to by buf. 
   If retain_newlines then the routine retains any newline '\n' 
   characters. */
extern PREFIX int Read_file(char *filename, char **buf, int *size, 
			    int retain_newlines);

#ifdef USE_REG_TIMING
/* Return the time since the epoch in seconds */
extern PREFIX int Get_current_time_seconds(double *now);
#endif

extern PREFIX int Reorder_decode_array(IOdef_entry *io,
				       int          type,
				       int          count,
				       void        *pData);

/* Does what is says.  Uses uname and gethostbyname. */
extern PREFIX int Get_fully_qualified_hostname(char **hostname, 
					       char **ip_addr_ptr);

#endif
