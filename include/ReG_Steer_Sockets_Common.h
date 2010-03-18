/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Robert Haines
 */

#ifndef __REG_STEER_SOCKETS_COMMON_H__
#define __REG_STEER_SOCKETS_COMMON_H__

/** @file ReG_Steer_Sockets_Common.h
 *  @brief Data structures and routines common to all sockets-based code.
 *
 *  @author Robert Haines
 */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"

#define REG_SOCKETS_ERROR -1

/** @internal
    Structure to hold socket information */
typedef struct {
  /** Minimum port number we can use to listen on (zero if any) */
  int                   min_port_in;
  /** Maximum port number we can use to listen on (zero if any) */
  int                   max_port_in;
  /** Minimum port number we can use to connect out of (zero if any) */
  int                   min_port_out;
  /** Maximum port number we can use to connect out of (zero if any) */
  int                   max_port_out;
  /** Default outbound tcp interface */
  char			tcp_interface[REG_MAX_STRING_LENGTH];
  /** Handle of listener socket - info for socket connection ("server" end) */
  int			listener_handle;
  /** Hostname on which to listen -  info for socket connection ("server" end) */
  char			listener_hostname[REG_MAX_STRING_LENGTH];
  /** Port on which to listen -  info for socket connection ("server" end) */
  unsigned short int	listener_port;
  /** Handle of connecting socket - info for socket connection ("client" end) */
  int			connector_handle;
  /** Hostname to connect to - info for socket connection ("client" end) */
  char			connector_hostname[REG_MAX_STRING_LENGTH];
  /** Port to connect to - info for socket connection ("client" end) */
  unsigned short int	connector_port;
  /** status indicator for listening socket */
  int			listener_status;
  /** status indicator for connecting socket */
  int			comms_status;
} socket_info_type;

typedef struct {
  int max_entries;
  int num_used;
  socket_info_type* socket_info;
} socket_info_table_type;

/** @internal
    @param table Pointer to the table of socket information to be
    initialised
    @param max_entries The maximum size of that table

    Initialise socket info table.
 */
int socket_info_table_init(socket_info_table_type* table,
			   const int max_entries);

/** @internal
    @param socket_info Pointer to the socket information for the socket
    to be initialised

    Initialise socket_info_type structure */
int socket_info_init(socket_info_type* socket_info);

/** @internal
    @param socket_info Pointer to the socket information for the socket
    to be cleaned up.

    Clean up members of socket_info_type structure */
void socket_info_cleanup(socket_info_type* socket_info);

/** @internal
    @param hostname Fully qualified name of machine to look-up.
    @param ipaddr On successful return holds the IP address of
    the machine.
    @param canon If true (non-zero), on successful return
    @p hostname will have been modified to hold the canonical
    name of the machine.
    @return REG_SUCCESS, REG_FAILURE

    Looks up the IP of the specified @p hostname */
int dns_lookup(char* hostname, char* ipaddr, int canon);

/** @internal
    @param s File descriptor of the receiving socket
    @param buf Pointer to buffer in which to put received data (must
    be at least @p len in size)
    @param len Number of bytes to read from socket
    @param flags Flags to pass to the underlying call to recv()

    A wrapper around the recv() call to do a non-blocking receive.
    See recv(2) and fcntl(2). */
ssize_t recv_non_block(int s, void *buf, size_t len, int flags);

#if !REG_HAS_MSG_NOSIGNAL
/** @internal
    Handler for SIGPIPE generated when connection goes down.
    Only defined for systems without MSG_NOSIGNAL.  Necessary because
    otherwise the signal takes down the program. */
void signal_handler_sockets(int a_signal);
#endif

/** @internal
    @param s File descriptor of the sending socket
    @param buf Pointer to buffer from which to send data (must
    be at least @p len in size)
    @param len Number of bytes to send through socket
    @param flags Flags to pass to the underlying call to send()

    Wrap send() so we can avoid lots of \#ifdefs in the main body of code.
    See send(2). */
ssize_t send_no_signal(int s, const void *buf, size_t len, int flags);

/** @internal
    @param s File descriptor of the receiving socket
    @param buf Pointer to buffer in which to put received data (must
    be at least @p len in size)
    @param len Number of bytes to read from socket
    @param flags Flags to pass to the underlying call to recv()

    A wrapper around the recv() call to do a blocking receive.
    See recv(2). */
ssize_t recv_wait_all(int s, void *buf, size_t len, int flags);

/** @internal
    @param s File descriptor of the socket to set.

    A wrapper around the setsockopt() call to set SO_REUSEADDR in a
    cross-platform (ie MSVC) manner. See setsockopt(2). */
int set_reuseaddr(int s);

/** @internal
    @param s File descriptor of the socket to set.

    A wrapper around the setsockopt() call to set TCP_NODELAY in a
    cross-platform (ie MSVC) manner. See setsockopt(2). */
int set_tcpnodelay(int s);

#endif /* __REG_STEER_SOCKETS_COMMON_H__ */
