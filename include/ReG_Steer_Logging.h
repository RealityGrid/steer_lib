/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  logging-related activity.

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
#ifndef __REG_STEER_APPSIDE_LOGGING_H__
#define __REG_STEER_APPSIDE_LOGGING_H__


/* Emit any log entries that steerer doesn't know about.  The handle
   argument is used to identify which parameter log to emit if the
   log is of the appropriate type. */
int Emit_log(Chk_log_type *log, int handle);

/* Sets the value of the next primary key to be used in generating
   log entries.  If a log file exists then it pulls the last value out
   of it and increments it by one, otherwise it sets it to zero. */
int Set_log_primary_key(Chk_log_type *log);

/* Allocate memory etc. for the log structure supplied */
int Initialize_log(Chk_log_type *log);

/* Identifies format of log supplied in *buf & converts it to xml if
   necessary.  Then calls Pack_send_log_entries. */
int Emit_log_entries(Chk_log_type *log, char *buf);

/* Open log file (in append mode) */
int Open_log_file(Chk_log_type *log);

/* Close the log file */
int Close_log_file(Chk_log_type *log);

/* Save current contents of log to file */
int Save_log(Chk_log_type *log);

/* Convert current log to xml and store in buffer pointed to by pchar.
   Length of buffer is returned in count. The memory pointed to by
   *pchar must be free()'d by the caller once finished.  If
   not_sent_only == REG_TRUE then only those entries not already returned
   to the steering client are retrieved */
int Log_to_xml(Chk_log_type *log, char **pchar, int *count, 
		      const int not_sent_only);

/* Called from Log_to_xml for Checkpoint logs */
int Chk_log_to_xml(Chk_log_type *log, char **pchar, int *count, 
		   const int not_sent_only);

/* Called from Log_to_xml for Parameter logs */
int Param_log_to_xml(Chk_log_type *log, char **pchar, int *count, 
		     const int not_sent_only);

/* As for Log_to_xml except log is stored in columnar format - for
   storing parameter histories. */
int Log_to_columns(Chk_log_type *log, char **pchar, int *count, 
		   const int not_sent_only);

/* Convert a columnar-format log back into xml.  buf points to
   the columnar data (space delimited data on lines delimited by
   new-line chars) and out_buf points to a buffer pre-allocated
   to receive the new format log. */
int Log_columns_to_xml(char **buf, char* out_buf, int out_buf_size);

/* Takes the xml document describing a log and splits it into separate
   entries which are packed into messages and sent to client */
int Pack_send_log_entries(char **pBuf, int *msg_count);

#endif