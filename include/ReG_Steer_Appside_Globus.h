/*----------------------------------------------------------------------------
    This header file contains routines and data structures for
    socket communication using Globus IO.

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

    Initial version by:  S Ramsden, 26.2.2003       0.1               

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_APPSIDE_GLOBUS_H__
#define __REG_STEER_APPSIDE_GLOBUS_H__

#if REG_GLOBUS_STEERING

int Send_status_msg_globus(char *buf);

struct msg_struct *Get_control_msg_globus();

int Initialize_steering_connection_globus(int  NumSupportedCmds,
						 int *SupportedCmds);

int Steerer_connected_globus();

int Finalize_steering_connection_globus();

#endif

#if REG_GLOBUS_SAMPLES

int Initialize_IOType_transport_globus(const int direction,
				       const int index);

void Finalize_IOType_transport_globus();

int Consume_start_data_check_globus(const int index);

int Consume_data_read_globus(const int		index,  
			     const int		datatype,
			     const size_t	num_bytes_to_send, 
			     void		*pData);

int Emit_header_globus(const int index);

int Emit_footer_globus(const int index,
 		       const char * const buffer);

int Emit_data_globus(const int		index,
		     const int		datatype,
		     const size_t	num_bytes_to_send,
		     void		*pData);


int Get_communication_status_globus(const int index);

#endif


#endif
