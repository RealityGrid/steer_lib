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

#ifndef __REG_STEER_SAMPLES_TRANSPORT_SOCKETS_H__
#define __REG_STEER_SAMPLES_TRANSPORT_SOCKETS_H__

/** @file ReG_Steer_Samples_Transport_Sockets.h
 *  @brief Socket specific routines for the samples transport module.
 *
 *  @author Robert Haines
 */

#include "ReG_Steer_types.h"
#include "ReG_Steer_Sockets_Common.h"

/** @internal
    @param index Index of the IOType to which socket belongs

    Create a listening socket */
int create_listener_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Create a connector socket */
int create_connector_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Sets up and then attempts to connect a connector */
int connect_connector_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Take down a listener */
void cleanup_listener_connection_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Take down a connector */
void cleanup_connector_connection_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Calls close on the listener handle */
void close_listener_handle_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Calls close on the connector handle */
void close_connector_handle_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Checks to see if anyone is trying to connect */
void attempt_listener_connect_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Cleans-up a broken socket and tries to reconnect */
void retry_accept_connect_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Attempts to reconnect a connector */
void attempt_connector_connect_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Takes down a failed connector and tries again */
void retry_connect_samples(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Checks socket connection and tries to establish
    connection if none (whether listener or connector) */
void poll_socket_samples(const int index);

#endif /* __REG_STEER_SAMPLES_TRANSPORT_SOCKETS_H__ */
