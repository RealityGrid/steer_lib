/*----------------------------------------------------------------------------
    This header file contains routines and data structures for
    steerside socket communication using Globus IO.

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
#ifndef __REG_STEER_STEERSIDE_GLOBUS_H__
#define __REG_STEER_STEERSIDE_GLOBUS_H__

#include "ReG_Steer_Steerside_internal.h"

#if REG_GLOBUS_STEERING

/* Attach to specified simulation using globus_io */
int Sim_attach_globus(Sim_entry_type *sim, char *SimID);

int Consume_supp_cmds_globus(Sim_entry_type *sim);

int Send_control_msg_globus(int SimIndex, char* buf);

struct msg_struct *Get_status_msg_globus(Sim_entry_type *sim);

int Finalize_connection_globus(Sim_entry_type *sim);

#endif
#endif
