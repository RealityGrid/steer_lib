/*----------------------------------------------------------------------------
  Header file containing prototypes and datatype definitions for
  entities that are common to both the application- and steerer-side
  of the RealityGrid steering library.

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

---------------------------------------------------------------------------*/

/** @file ReG_Steer_Common.h
    @brief Header file for routines used in both Appside and Steerside
    @author Andrew Porter
    @author Robert Haines
  */

#ifndef __REG_STEER_COMMON_H__
#define __REG_STEER_COMMON_H__

#include "ReG_Steer_Appside_Sockets.h"
#include "soapH.h"

/* Following two includes are for use of stat system call 
   in Open_next_file */
#include <sys/types.h>
#include <sys/stat.h>

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

/** 
  Used to log parameter values 
 */
typedef struct {

  int  handle;
  char value[REG_MAX_STRING_LENGTH];

} Param_log_entry_type;

/** 
  Used to log steering commands 
 */
typedef struct {

  int   id;
  char  params[REG_MAX_STRING_LENGTH];

} Cmd_log_entry_type;

/** 
  Used to log steering activity (both commands and parameter changes) 
 */
typedef struct {

  /* For parameter value changes */
  int   num_params;
  Param_log_entry_type param[REG_MAX_NUM_STR_PARAMS];
  /* For other cmds */
  int   num_cmds;
  Cmd_log_entry_type cmd[REG_MAX_NUM_STR_CMDS];

} Steer_log_type;

/** 
  Used to hold information on a single checkpoint 
 */
typedef struct {

  int                  key;
  int                  chk_handle;
  char                 chk_tag[REG_MAX_STRING_LENGTH];
  int                  num_param;
  Param_log_entry_type param[REG_MAX_NUM_STR_PARAMS];
  int                  sent_to_steerer;

} Chk_log_entry_type;

typedef enum {PARAM, CHKPT} log_type_type ;

/** 
  The checkpoint log 
 */
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


/**
  Holds information on a single parameter
 */
typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   steerable;
  int   type;
  int   handle;
  void *ptr;
  void *ptr_raw;
  unsigned int raw_buf_size;
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
  int     logging_on;

} param_entry;

/**
  Holds information on all registered parameters
 */
typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  int          log_all;
  param_entry *param;

} Param_table_type;

/** 
  Information on a single supported command 
*/
typedef struct {

  int               cmd_id;
  Param_table_type  cmd_params;

}supp_cmd_entry;

/** 
  Information on all supported commands
*/
typedef struct {

  int             num_registered;
  int             max_entries;
  supp_cmd_entry *cmd;

} Supp_cmd_table_type;

/**
  Type definition for variable describing block distribution of
  slices being emitted
 */
typedef struct {

  /** Whether array data is in F90 ordering */
  int is_f90;
  /** Extent of whole array */
  int totx, toty, totz;
  /** Extent of sub-array */
  int nx, ny, nz;
  /** Origin of sub-array within whole */
  int sx, sy, sz;

} Array_type;

/**
  Description of a single IOType
 */
typedef struct {
  /** Label of IOType (supplied by user) */
  char				label[REG_MAX_STRING_LENGTH];
  /** Handle of IOType - generated by us & returned to user */
  int				handle;

  /** For file-based IO */
  char				filename[REG_MAX_STRING_LENGTH];
  /** For file-based IO */
  char                          directory[REG_MAX_STRING_LENGTH];
  /** For file-based IO */
  FILE                         *fp;

  /** Whether input, output */
  int				direction;
  /** Variable holding the frequency with which emission/consumption is
     to occur (every frequency steps) */
  int                           frequency;
  /** Handle of the (steerable) frequency in the parameter table */
  int				freq_param_handle;

  /** Pointer to buffer to hold data */
  void			       *buffer;
  /** How much data there is currently in the buffer */
  int                           buffer_bytes;
  /** Size of this buffer */
  int				buffer_max_bytes;
#if REG_SOCKET_SAMPLES
  /** structure used to hold all socket information */
  socket_io_type		socket_info;
#endif
  /** Whether or not to encode non-ASCII data as XDR (set in Emit_start) */
  int				use_xdr;
  /** How many bytes of xdr data to read (to avoid having to work-out/guess
     how many it will be from the type) */
  int                           num_xdr_bytes;
  /** Details on incoming array (if available) */
  Array_type                    array;
  /** Whether or not (1 or 0) we'll need to convert the ordering of 
     the array */
  int                           convert_array_order;
  /** Whether IOType is enabled or not (for sockets - whether socket
     has been created) */
  int                           is_enabled;
  /** Index of the input channel - used in mapping to the details
     held by the SGS (when steering via SOAP) */
  int                           input_index;
  /** Whether or not this IOType (direction OUT) cares about getting 
     an acknowledgement before trying to emit the next sample */
  int                           use_ack;
  /** Whether or not we need to check for an acknowledgement before
     attempting to emit the next data set.  'use_ack' OVERRIDES
     this flag. */
  int                           ack_needed;

} IOdef_entry;

/** 
  Table holding all IOTypes 
 */
typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
/** Whether or not to create the socket
    (if using them) when an IOType
    is registered */  
  int          enable_on_registration; 
 /** Count of input channels registered - used to
     map to details on data sources held by SGS */
  int          num_inputs;
  IOdef_entry *io_def;

}IOdef_table_type;

/** For steering via SOAP - holds info on the Grid service */
typedef struct {
    /** whether we're steering via SOAP (1) or not (0) */
    int  active;
    /** Location of the Grid service */
    char address[REG_MAX_STRING_LENGTH];
    /** Holds list of names of service data elements on the SGS for which
	notifications are pending */
    char notifications[REG_MAX_NUM_SGS_SDE][REG_MAX_STRING_LENGTH];
    /** Used to keep track of notifications that we've yet to process */
    int  sde_count;
    int  sde_index;
    /** The stucture holding the gSOAP environment for this connection */
    struct soap *soap;
    /** Username to use with WS-Security with this service */
    char username[REG_MAX_STRING_LENGTH];
    /** the passphrase (if any) used with WS-Security for this service */
    char passwd[REG_MAX_STRING_LENGTH];

  } SGS_info_type;

/** Holds details required for secure access to a service
    using either SSL or WSSE */
struct reg_security_info {
  /** Whether or not this structure contains info. for use with
      ssl (REG_TRUE) or WSSE (REG_FALSE) */
  int  use_ssl;
  /** Path to directroy containing CA certificates (ssl) */
  char caCertsPath[REG_MAX_STRING_LENGTH];
  /** Full path to pem file containing user's key and certificate
      concatenated together (ssl) */
  char myKeyCertFile[REG_MAX_STRING_LENGTH];
  /** The user's DN or other username (ssl or WSSE) */
  char userDN[REG_MAX_STRING_LENGTH];
  /** Passphrase (ssl - for key, wsse - for service) */
  char passphrase[REG_MAX_STRING_LENGTH];
};

/*-------- Function prototypes --------*/

/**
  Searches for and opens the next file in a numbered
  sequence with the specified root name
 */
extern PREFIX FILE *Open_next_file(char* base_name);

/**
  Creates a lock file with name consisting of ".lock"
  appended to supplied name
 */
extern PREFIX int Create_lock_file(char *filename);

/**
  Delete the specified file (must have full path)
 */
extern PREFIX int Delete_file(char *filename);

/**
  Checks whether the specified directory is valid and that we
  can write to it
 */
extern PREFIX int Directory_valid(char *directory);

/** 
   Routine to get (& check validity of) steering scratch directory
   from REG_STEER_DIRECTORY env. variable.  Used even when steering
   is NOT file based. */
extern PREFIX int Set_steering_directory();

/**
   Looks up the next free index in the parameter table (might not
   be at the end because parameters can, in theory, be deleted).
 */
extern PREFIX int Next_free_param_index(Param_table_type *table);

/** 
   A look-up function - return the index of the parameter with handle
   ParamHandle in the table pointed to by *table. Returns 
   REG_PARAM_HANDLE_NOTSET if no matching handle found. */
extern PREFIX int Param_index_from_handle(Param_table_type *table, 
				   int ParamHandle);

/**
   Initializes the supplied parameter entry. */
extern PREFIX void Init_param_entry(param_entry *param);

/** 
   A look-up function - return the index of the IOdef with handle 
   IOdefHandle in the table pointed to by *table.  Returns
   REG_IODEF_HANDLE_NOTSET if no matching handle found. */
extern PREFIX int IOdef_index_from_handle(IOdef_table_type *table, 
					  int IOdefHandle);

/**
  Increment the count of parameters registered and allocate
  more memory if required.
 */
extern PREFIX int Increment_param_registered(Param_table_type *table);

/**
  Increment the count of commands registered and allocate
  more memory if required.
 */
extern PREFIX int Increment_cmd_registered(Supp_cmd_table_type *table);

/**
  Increment the count of IOTypes registered and allocate
  more memory if required.
 */
extern PREFIX int Increment_iodef_registered(IOdef_table_type *table);

/**
  Increment the count of log entries and allocate
  more memory if required.
 */
extern PREFIX int Increment_log_entry(Chk_log_type *log);

/** 
  Called when steering finished - cleans up any files that either the app
  or steerer hasn't got around to consuming 
 */
extern PREFIX int Remove_files(char* base_name);

/**
  Enquiry function - converts from XML tag name to ENUM value
 */
extern PREFIX int Get_message_type(const char *name);

/** 
  Write ReG-specific XML header information into supplied message
  buffer 
  @param pchar Pointer to buffer in which to put header
  @internal */
extern PREFIX int Write_xml_header(char **pchar); 

/** 
  Write ReG-specific XML footer information into supplied message
  buffer */
extern PREFIX int Write_xml_footer(char **pchar,
				   int bytes_free);

/** 
   Read the specified ASCII file and return the contents in the buffer
   pointed to by *buf, the size of which is returned in *size.  It is
   the caller's responsibility to free() the memory pointed to by buf. 
   @param retain_newlines if REG_TRUE then routine retains any newline '\n' 
   characters. */
extern PREFIX int Read_file(const char *filename, 
			    char **buf, int *size, 
			    const int retain_newlines);

/** Return the time since the epoch in seconds */
extern PREFIX int Get_current_time_seconds(double *now);

/**
  Intended to take array of data and re-order (from F90 to C or
  vice versa) and decode it (from XDR).  Not used.
 */
extern PREFIX int Reorder_decode_array(IOdef_entry *io,
				       int          type,
				       int          count,
				       void        *pData);

/** Does what is says.  Uses uname and gethostbyname. */
extern PREFIX int Get_fully_qualified_hostname(char **hostname, 
					       char **ip_addr_ptr);

/** Initialize the OpenSSL random number generator for this thread 
    @internal */
extern PREFIX int Init_random();

/** Creates a WSRF header including WS-Security elements  for gSoap 
    (within the supplied soap struct). If @p username is null or
    is empty then no WS-Security elements are created.
    @internal
    @param aSoap Pointer to soap struct to construct header in
    @param epr The address of the service to be called
    @param username The username to present to the service
    @param passwd The password used to access the service
    @returns REG_SUCCESS or REG_FAILURE if no header created */
extern PREFIX int Create_WSRF_header(struct soap *aSoap,
				     const  char *epr,
				     const  char *username,
				     const  char *passwd);

/** Return the current (GMT) date and time as a string in the format
    YYYY-MM-DDTHH:MM:SSZ suitable for inclusion in XML documents */
extern PREFIX char *Get_current_time_string();

/** Initialize the SSL context for the supplied gSoap structure
    @param aSoap Ptr to soap struct to be initialized
    @param authenticateSWS Whether or not to verify the cert. presented by the SWS.  If REG_TRUE then caCertPath must be set.
    @param certKeyPemFile Full path to file containing user's certificate
           and key (if doing mutual authentication, NULL otherwise) 
    @param passphrase Passphrase for the user's key (can be NULL if not
           doing mutual authentication)
    @param caCertPath Path to directory containing CA certs 
    @return REG_SUCCESS or REG_FAILURE */
extern PREFIX int REG_Init_ssl_context(struct soap *aSoap,
				       const int    authenticateSWS,
				       const char  *certKeyPemFile,
				       const char  *passphrase,
				       const char  *caCertPath);

/** Resets all entries in the struct
    @param sec Pointer to the reg_security_info structure to reset */
extern PREFIX void Wipe_security_info(struct reg_security_info *sec);

#endif
