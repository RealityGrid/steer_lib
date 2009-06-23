/*----------------------------------------------------------------------------
  (C) Copyright 2009, University of Manchester, United Kingdom,
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

#ifndef __REG_STEER_SAMPLES_TRANSPORT_SOCKETS_H__
#define __REG_STEER_SAMPLES_TRANSPORT_SOCKETS_H__

#include "ReG_Steer_Config.h"
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
