/*----------------------------------------------------------------------------
    This header file contains routines and data structures for
    steerside SOAP-based communication.

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
#ifndef __REG_STEER_STEERSIDE_SOAP_H__
#define __REG_STEER_STEERSIDE_SOAP_H__

/* These NEED TO MATCH the names given to the service data elements in
   SGS.pm */
#define REG_APP_STATUS_SDE "Application_status"
#define REG_STEER_STATUS_SDE "Steerer_status"

/*-------------------------------------------------------------------*/

/* Initialise soap-specific structures etc. */
int Steerer_initialize_soap();

/* Attach to simulation via SOAP */
int Sim_attach_soap(Sim_entry_type *sim, char *SimID);

int Send_control_msg_soap(Sim_entry_type *sim, char* buf);

int Send_detach_msg_soap(Sim_entry_type *sim);

int Send_stop_msg_soap(Sim_entry_type *sim);

int Send_pause_msg_soap(Sim_entry_type *sim);

int Send_resume_msg_soap(Sim_entry_type *sim);

struct msg_struct *Get_status_msg_soap(Sim_entry_type *sim);

struct msg_struct *Get_service_data(Sim_entry_type *sim, char *sde_name);

#endif
