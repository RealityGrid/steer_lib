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
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include "ReG_Steer_types.h"

/*-----------------------------------------------------------------*/

/* Messages used to communicate with proxy steerer - these MUST
   MATCH the definitions in ReG_Steer_Proxy.java */

static char *END_OF_MSG     = "END_OF_MSG\n";
static char *OK_MSG         = "STATUS_OK\n";
static char *ERR_MSG        = "ERROR\n";
static char *ATTACH_MSG     = "ATTACH\n";
static char *DETACH_MSG     = "DETACH\n";
static char *QUIT_MSG       = "QUIT\n";
static char *GET_APPS_MSG   = "GET_APPS\n";
static char *GET_STATUS_MSG = "GET_STATUS\n";
static char *SEND_CTRL_MSG  = "SEND_CTRL\n";

/*-----------------------------------------------------------------*/

int Create_proxy(int *to_proxy, int *from_proxy);

int Destroy_proxy(int pipe_to_proxy);

int Send_proxy_message(int pipe_to_proxy, const char *buf);

int Get_proxy_message(int pipe_from_proxy, char *buf, int *nbytes);

/* Function to check that the specified java executable (assumed to
   need ".class" appending to it) is on the specified class path */
int Proxy_is_in_path(const char *class_path, const char *exec);
