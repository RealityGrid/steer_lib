/*----------------------------------------------------------------------------
    Header file containing prototypes and datatype definitions for
    entities that are common to both the application- and steerer-side
    of the RealityGrid steering library.

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

    Initial version by: A Porter, 12/8/2002.
    
---------------------------------------------------------------------------*/

#ifndef __REG_STEER_COMMON_H__
#define __REG_STEER_COMMON_H__

#include "ReG_Steer_Globus_io.h"

/* Following two includes are for use of stat system call 
   in Open_next_file */
#include <sys/types.h>
#include <sys/stat.h>

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

#if REG_GLOBUS_STEERING
typedef socket_io_type	socket_type_steering;
#else
typedef int		socket_type_steering;
#endif

/* Structures for checkpoint logging */

typedef struct {

  int  handle;
  char value[REG_MAX_STRING_LENGTH];

} Param_log_entry_type;

typedef struct {

  int                  key;
  int                  chk_handle;
  char                 chk_tag[REG_MAX_STRING_LENGTH];
  int                  num_param;
  Param_log_entry_type param[REG_MAX_NUM_STR_PARAMS];
  int                  sent_to_steerer;

} Chk_log_entry_type;

typedef struct {

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
  /* Flag to indicate whether or not all entries must be sent to
     steerer, IRRESPECTIVE of the value of their individual 
     sent_to_steerer flags.  This used when a client detaches and
     another one attaches some time later. */
  int                 send_all;
  
} Chk_log_type;


/* Types and structures for reading parameter defs */

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   steerable;
  int   type;
  int   handle;
  void *ptr;
  char  value[REG_MAX_STRING_LENGTH];
  int   modified;
  int   is_internal;
  int   min_val_valid;
  char  min_val[REG_MAX_STRING_LENGTH];
  int   max_val_valid;
  char  max_val[REG_MAX_STRING_LENGTH];

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
  char				label[REG_MAX_STRING_LENGTH];
  char				filename[REG_MAX_STRING_LENGTH];
  int				handle;
  /* Whether input, output */
  int				direction;
  /* Variable holding the frequency with which emission/consumption is
     to occur (every frequency steps) */
  int                           frequency;
  /* Handle of the (steerable) frequency in the parameter table */
  int				freq_param_handle;

  /* Pointer to buffer to hold data */
  void			       *buffer;
  /* Size of this buffer */
  int				buffer_bytes;
#if REG_GLOBUS_SAMPLES
  /* structure used to hold all globus_io socket information */
  socket_io_type		socket_info;
#endif
  /* Whether or not to encode non-ASCII data as XDR (set in Emit_start) */
  int				use_xdr;
  /* How many bytes of xdr data to read (to avoid having to work-out/guess
     how many it will be from the type) */
  int                           num_xdr_bytes;
  /* Whether array data is in F90 ordering */
  int                           is_f90_array;

} IOdef_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
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

extern PREFIX REG_MsgType Get_message_type(const char *name);

/* Write ReG-specific XML header & footer information into supplied message
   buffer */
extern PREFIX int  Write_xml_header(char **pchar);

extern PREFIX int  Write_xml_footer(char **pchar,
				    int bytes_free);

/* Read ReG-specific header */
extern PREFIX int Consume_msg_header(socket_type_steering *sock_info,
				     int *DataType,
				     int *Count);

/* Construct and send ReG-specific header */
extern PREFIX int Emit_msg_header(socket_type_steering *sock_info,
				  int DataType,
				  int Count);

/* Read the specified ASCII file and return the contents in the buffer
   pointed to by *buf, the size of which is returned in *size.  It is
   the caller's responsibility to free() the memory pointed to by buf. */
extern PREFIX int Read_file(char *filename, char **buf, int *size);

#ifdef USE_REG_TIMING
/* Return the time since the epoch in seconds */
extern PREFIX int Get_current_time_seconds(double *now);
#endif

#endif
