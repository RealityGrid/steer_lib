/*----------------------------------------------------------------------------
    Header file defining the routines used to create and communicate
    with a steering-proxy process.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the authors, nor the University of
    Manchester offer any warranties or representations, nor do they
    accept any liabilities with respect to this software.

    This program must not be used for commmercial gain without the
    written permission of the authors.
    
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL

    email:  csar-advice@cfs.ac.uk.
    Tel:    +44 161 275 6824/5997
    Fax:    +44 161 275 6040    
    
    Date          Version    Updates                            Author
    ----          -------    -------                            ------
    30.9.2002       0.1                                         A Porter

---------------------------------------------------------------------------*/

#include <sys/types.h>
#include <unistd.h>

#include "ReG_Steer_types.h"

int Create_proxy(int *to_proxy, int *from_proxy);

int Destroy_proxy(int pipe_to_proxy);

int Send_proxy_message(int pipe_to_proxy, const char *buf);

int Get_proxy_message(int pipe_from_proxy, char *buf, int *nbytes);
