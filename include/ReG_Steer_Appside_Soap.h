/*----------------------------------------------------------------------------
    This header file contains routines and data structures for
    SOAP-based steering communication.

    (C)Copyright 2003, The University of Manchester, United Kingdom,
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

    Initial version by:  A Porter, 23.4.2003       0.1               

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_APPSIDE_SOAP_H__
#define __REG_STEER_APPSIDE_SOAP_H__

/*------------------------------------------------------------------*/

int Initialize_steering_connection_soap(int  NumSupportedCmds,
					int *SupportedCmds);

int Steerer_connected_soap();

int Send_status_msg_soap(char* msg);

int Detach_from_steerer_soap();

struct msg_struct *Get_control_msg_soap();

int Finalize_steering_connection_soap();

int Get_data_source_address_soap(int                 index, 
				 char               *hostname,
				 unsigned short int *port);

int Record_checkpoint_set_soap(char *chk_data,
			       char *node_data);

#endif
