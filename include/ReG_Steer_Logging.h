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
#ifndef __REG_STEER_APPSIDE_LOGGING_H__
#define __REG_STEER_APPSIDE_LOGGING_H__

/** @internal
    @file ReG_Steer_Logging.h
    @brief Header file for logging functionality 
    This header file contains routines and data structures for
    logging-related activity.
    @author Andrew Porter
    @author Robert Haines */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_XML.h"

/** @internal
    @param log Pointer to log to emit
    @param handle Which parameter log to emit if @p is a parameter log

    Emit any log entries that steerer doesn't know about.  The handle
    argument is used to identify which parameter log to emit if the
    log is of the appropriate type. */
int Emit_log(Chk_log_type *log, int handle);

/** @internal
    @param log Pointer to log for which to set primary key

    Sets the value of the next primary key to be used in generating
    log entries.  If a log file exists then it pulls the last value out
    of it and increments it by one, otherwise it sets it to zero. */
int Set_log_primary_key(Chk_log_type *log);

/** @internal
    @param log Pointer to log to initialize
    @param log_type The type of log (whether PARAM or CHK)

    Allocate memory etc. for the log structure supplied.
    @p log->filename must be set prior to call. */
int Initialize_log(Chk_log_type *log, 
		   log_type_type log_type);

/** @internal
    @param log Pointer to log to emit (if a continuation of an emit)
    @param buf Buffer containing log information to emit
    @param handle Handle of parameter whose log to extract if log
    is of PARAM type
    @bug @p log argument no longer required?

    Identifies format of log supplied in *buf & converts it to xml if
    necessary.  If the log is a parameter log then it pulls out values
    of the parameter with the specified handle. Then calls 
    Pack_send_log_entries(). */
int Emit_log_entries(Chk_log_type *log, 
		     char         *buf, 
		     int           handle);

/** @internal
    @param log Pointer to log for which to open file

    Open log file (in append mode) */
int Open_log_file(Chk_log_type *log);

/** @internal
    @param log Pointer to log for which to close file

    Close the log file */
int Close_log_file(Chk_log_type *log);

/** @internal
    @param log Pointer to log to finalize

    Free's up memory associated with log */
int Finalize_log(Chk_log_type *log);

/** @internal
    @param log Pointer to log structure

    (Close and) delete the log file */
int Delete_log_file(Chk_log_type *log);

/** @internal
    @param log Pointer to log structure to save

    Save current contents of log to file and cache on SGS/SWS */
int Save_log(Chk_log_type *log);

/** @internal
    @param log Pointer to log structure to get data from
    @param handle Specifies which parameter to extract if log is of
    PARAM type
    @param pchar Pointer to buffer containing extracted log (must be
    free'd by caller)
    @param count No. of bytes of data returned in buffer pointed 
    to by @p pchar
    @param not_sent_only If set to REG_TRUE then only those entries not
    already sent to the steering client are retrieved

    Convert current log to xml and store in buffer pointed to by
    @p pchar.  Length of buffer is returned in @p count. The memory pointed
    to by @p pchar must be free()'d by the caller once finished.*/
int Log_to_xml(Chk_log_type *log, 
	       int           handle, 
	       char        **pchar, 
	       int          *count, 
	       const int     not_sent_only);

/** @internal
    @param log Pointer to log structure to get data from
    @param pchar Pointer to buffer for extracted log data
    @param count No. of bytes of data returned in @p pchar
    @param not_sent_only If set to REG_TRUE then only those entries not
    already sent to the steering client are retrieved

    Called from Log_to_xml() for Checkpoint logs */
int Chk_log_to_xml(Chk_log_type *log, 
		   char        **pchar, 
		   int          *count, 
		   const int     not_sent_only);

/** @internal
    @param log Pointer to log structure to get data from
    @param handle Handle of parameter for which to extract log data
    @param pchar On return, pointer to buffer containing extracted data
    @param count No. of bytes in @p pchar
    @param not_sent_only If set to REG_TRUE then only those entries not
    already sent to the steering client are retrieved
    
    Called from Log_to_xml() to get xml representation of a parameter log. */
int Param_log_to_xml(Chk_log_type *log, int handle, char **pchar, 
		     int *count, const int not_sent_only);

/** @internal
    @param log Pointer to log structure from which to get data
    @param pchar Pointer to buffer containing results (must be free'd 
    by caller)
    @param count No. of bytes of data returned in @p pchar
    @param not_sent_only If set to REG_TRUE then only those entries not
    already sent to the steering client are retrieved

    As for Log_to_xml() except log is stored in columnar format - for
    storing parameter histories. */
int Log_to_columns(Chk_log_type *log, 
		   char        **pchar, 
		   int          *count, 
		   const int     not_sent_only);

/** @internal Log a snapshot of the current parameter values */
int Log_param_values();

/** @internal
    @param buf Points to the columnar data (space delimited data on 
    lines delimited by new-line chars)
    @param out_buf Points to a buffer pre-allocated to receive the 
    new format log
    @param out_buf_size Size of @p out_buf in bytes
    @param handle Specifies which parameter to pull out

    Convert a columnar-format log back into XML. */
int Log_columns_to_xml(char **buf, 
		       char  *out_buf, 
		       int    out_buf_size, 
		       int    handle);

/** @internal
    @param pBuf Pointer to buffer containing XML describing a log
    @param msg_count On return, holds the no. of distinct log messages
    that were sent to the steering client.
    
    Takes the xml document describing a log and splits it into separate
    entries which are packed into messages and sent to client */
int Pack_send_log_entries(char **pBuf, int *msg_count);

/** @internal
    @param control The control message to log

    Log the supplied control message - is stored in the Chk_log
    structure */
int Log_control_msg(struct control_struct *control);

#endif
