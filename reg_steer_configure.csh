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
setenv REG_STEER_HOME $HOME/reg_steer_lib

# Set directory location to use for steering communication
setenv REG_STEER_DIRECTORY /usr/tmp/zzcguap/

# Set location of XML parser library and header files
setenv REG_XML_LIBDIR /usr/lib
setenv REG_XML_INCDIR /usr/include/libxml2

setenv REG_CONNECTOR_HOSTNAME  vermont.mvc.mcc.ac.uk
setenv REG_CONNECTOR_PORT      4567

# Location of steering grid service that application will attach to
setenv REG_SGS_ADDRESS http://vermont.mvc.mcc.ac.uk:50005/

# Handle of registry to bind to
setenv REGISTRY_GSH http://localhost:8888/Unicore/Registry

# Location of ReG grid service
setenv REG_GS ${HOME}/projects/ReG-grid-service


# Additions to java classpath for grid service and steering proxy
#setenv CLASSPATH ${CLASSPATH}:${HOME}/bin
#setenv CLASSPATH ${CLASSPATH}:${REG_GS}
#setenv CLASSPATH ${CLASSPATH}:${REG_GS}/ajo.jar

