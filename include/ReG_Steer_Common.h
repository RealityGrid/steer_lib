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
#include "ReG_Steer_Globus_io.h"

/* Following two includes are for use of stat system call 
   in Open_next_file */

#include <sys/types.h>
#include <sys/stat.h>

#if REG_GLOBUS_STEERING
typedef socket_io_type	socket_type_steering;
#else
typedef int		socket_type_steering;
#endif

/* Types and structures for reading parameter defs */

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   steerable;
  int   type;
  int   handle;
  void *ptr;
  char  value[REG_MAX_STRING_LENGTH];
  char  max_val[REG_MAX_STRING_LENGTH];
  char  min_val[REG_MAX_STRING_LENGTH];
  int   modified;
  int   is_internal;

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
  /* Whether input, output or a checkpoint */
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

} IOdef_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  IOdef_entry *io_def;

}IOdef_table_type;

/*-------- Function prototypes --------*/

extern FILE *Open_next_file(char* base_name);

extern int Create_lock_file(char *filename);

extern int Delete_file(char *filename);

extern int Directory_valid(char *directory);

extern int Next_free_param_index(Param_table_type *table);

/* A look-up function - return the index of the parameter with handle
   ParamHandle in the table pointed to by *table. Returns 
   REG_PARAM_HANDLE_NOTSET if no matching handle found. */
extern int Param_index_from_handle(Param_table_type *table, int ParamHandle);

/* A look-up function - return the index of the IOdef with handle 
   IOdefHandle in the table pointed to by *table.  Returns
   REG_IODEF_HANDLE_NOTSET if no matching handle found. */
extern int IOdef_index_from_handle(IOdef_table_type *table, int IOdefHandle);

extern int Increment_param_registered(Param_table_type *table);

extern int Increment_cmd_registered(Supp_cmd_table_type *table);

extern int Increment_iodef_registered(IOdef_table_type *table);

/* Called when steering finished - cleans up any files that either the app
   or steerer hasn't got around to consuming */

extern int Remove_files(char* base_name);

extern REG_MsgType Get_message_type(const char *name);

/* Write ReG-specific XML header & footer information into supplied message
   buffer */
extern int  Write_xml_header(char **pchar);

extern int  Write_xml_footer(char **pchar);

/* Read ReG-specific header */
extern int Consume_msg_header(socket_type_steering *sock_info,
			      int *DataType,
			      int *Count);

/* Construct and send ReG-specific header */
extern int Emit_msg_header(socket_type_steering *sock_info,
			   int DataType,
			   int Count);

