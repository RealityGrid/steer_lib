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

#ifndef __REG_STEER_SAMPLES_TRANSPORT_API_H__
#define __REG_STEER_SAMPLES_TRANSPORT_API_H__

/** @file ReG_Steer_Samples_Transport_API.h
 *  @brief The API specification for the samples transport modules.
 *
 *  @author Robert Haines
 */

#include "ReG_Steer_Config.h"

#include "ReG_Steer_types.h"

/*-------- Function prototypes --------*/

/** @internal

    Do whatever is necessary to set up this particular transport module.
 */
int Initialize_samples_transport();

/** @internal

    Do whatever is necessary to clean up this particular transport module.
 */
int Finalize_samples_transport();

/** @internal
    @param direction Whether connecting (REG_IO_OUT) or listening (REG_IO_IN)
    @param index Index of the IOType to initialize

    Initialize the socket connection for an IOType */
int Initialize_IOType_transport_impl(const int direction,
				     const int index);

/** @internal
    Take down the socket connections for @e all registered IOTypes */
void Finalize_IOType_transport_impl();

/** @internal
    @param index Index of the IOType to enable

    Creates a socket for an IOType.  Only necessary if the IOType has
    been disabled or Enable_IOTypes_on_registration() has been called
    such that the associated socket was not created when the IOType
    was registered.
    @see Enable_IOTypes_on_registration() */
int Enable_IOType_impl(const int index);

/** @internal
    @param index Index of the IOType to disable

    Destroys the socket for the IOType with the supplied index. */
int Disable_IOType_impl(const int index);

/** @internal
    @param index Index of the IOType to query
    @return REG_SUCCESS if socket is connected.

    Queries the status of the connection of an IOType */
int Get_communication_status_impl(const int index);

/** @internal
    A non-blocking version of Write().
    Uses a select call to check the status of the socket
    before attempting to write to it.
    @see Emit_data() */
int Emit_data_non_blocking_impl(const int index, const int size,
				void* buffer);

/** @internal
    @param index Index of IOType to write header to

    Emits a header message on the socket for the
    IOType with the supplied index. */
int Emit_header_impl(const int index);

/** @internal
    @param index Index of IOType to use to send data
    @param num_bytes_to_send No. of bytes of data to send
    @param pData Pointer to buffer containing data to send
*/
int Emit_data_impl(const int index, const size_t num_bytes_to_send,
		   void* pData);

/** @internal
    @param index Index of IOType from which to get header data
    @param datatype On successful return, the type of the data in
    the following slice
    @param count On successful return, the no. of data objects in
    the following slice
    @param num_bytes On succesful return, the no. of bytes of data in
    the following slice
    @param is_fortran_array On successful return, whether the data in
    the following slice is from a fortran array (changes its ordering)

    Reads a message header from the socket for the
    IOType with the supplied index. */
int Consume_msg_header_impl(int  index,
			    int* datatype,
			    int* count,
			    int* num_bytes,
			    int* is_fortran_array);

int Emit_msg_header_impl(const int    index,
			 const size_t num_bytes_to_send,
			 void*        pData);

/** @internal
    @param index Index of IOType to check for data

    Check to see whether data is available on the socket
    for the IOType. */
int Consume_start_data_check_impl(const int index);

/** @internal
    @param index Index of the IOType to get data from
    @param datatype Type of data to read from the IOType
    @param num_bytes_to_read No. of bytes of data to read
    @param pData Pointer to buffer large enough to receive data

    Reads the specified amount of data off the socket associated with
    the IOType with the supplied index.  Calls recv to read the
    data. */
int Consume_data_read_impl(const int index,
			   const int datatype,
			   const int num_bytes_to_read,
			   void* pData);

/** @internal
    @param index Index of the IOType on which to send acknowledgement

    Acknowledge that a data set has been received
    and processed successfully on an IOType.*/
int Emit_ack_impl(const int index);

/** @internal
    Attempt to read an acknowledgement from the consumer of the
    IOType with the supplied index.*/
int Consume_ack_impl(const int index);

/** @internal
    This is quite possibly a massive hack...
 */
int Get_IOType_address_impl(int index, char** pbuf, int* bytes_left);

int Emit_start_impl(int index, int seqnum);

int Emit_stop_impl(int index);

int Consume_stop_impl(int index);

#endif /* __REG_STEER_SAMPLES_TRANSPORT_API_H__ */
