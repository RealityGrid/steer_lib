#    File to set up environment for RealityGrid steering
#
#    (C)Copyright 2002 The University of Manchester, United Kingdom,
#    all rights reserved.
#
#    This software is produced by the Supercomputing, Visualization &
#    e-Science Group, Manchester Computing, the Victoria University of
#    Manchester as part of the RealityGrid project.
#
#    This software has been tested with care but is not guaranteed for
#    any particular purpose. Neither the copyright holder, nor the
#    University of Manchester offer any warranties or representations,
#    nor do they accept any liabilities with respect to this software.
#
#    This software must not be used for commercial gain without the
#    written permission of the authors.
#    
#    This software must not be redistributed without the written
#    permission of the authors.
#
#    Permission is granted to modify this software, provided any
#    modifications are made freely available to the original authors.
# 
#    Supercomputing, Visualization & e-Science Group
#    Manchester Computing
#    University of Manchester
#    Manchester M13 9PL
#    
#    WWW:    http://www.sve.man.ac.uk  
#    email:  sve@man.ac.uk
#    Tel:    +44 161 275 6095
#    Fax:    +44 161 275 6800    
#
#    Initial version by: Andrew Porter, 25.10.2002.
#---------------------------------------------------------------------

# Set root directory for library installation
setenv REG_STEER_HOME $HOME/projects/reg_steer_lib

# Set directory location to use for steering communication
setenv REG_STEER_DIRECTORY /scratch/zzcguap/steering

# Minimum time interval (integer no. of seconds) between checks on 
# whether a steering client has connected.  If set to zero then a 
# check is performed on every call to Steering_control.  If unset
# then a default value (set in ReG_Steer_types.h) is used.
setenv REG_APP_POLL_INTERVAL 5

# Set location of XML parser library and header files
setenv REG_XML_LIBDIR /usr/freeware/lib
setenv REG_XML_INCDIR /usr/freeware/include/libxml2

#---------------------------------------------------------------
# This section only necessary if using globus_io- rather
# than file-based data transfer

# For Globus_io connections - need to set port to that reported
# by library when application launched
setenv REG_CONNECTOR_HOSTNAME  vermont.mvc.mcc.ac.uk
setenv REG_CONNECTOR_PORT      2566
setenv GLOBUS_TCP_PORT_RANGE   2566,2666

#---------------------------------------------------------------
# This section needed when steering via SOAP and the Steering
# Grid Service (or SGS).

# Location of steering grid service that application will attach to
setenv REG_SGS_ADDRESS http://vermont.mvc.mcc.ac.uk:50005/

