# File to set up environment for RealityGrid steering
# Andrew Porter, 25.10.2002.

# Set root directory for library installation
setenv REG_STEER_HOME $HOME/scratch/reg_steer_lib

# Set directory location to use for steering communication
setenv REG_STEER_DIRECTORY /scratch/zzcguap/steering/

# Set location of XML parser library and header files
setenv REG_XML_LIBDIR /usr/freeware/lib
setenv REG_XML_INCDIR /usr/freeware/include/libxml2

#---------------------------------------------------------------
#       This section only necessary if using globus_io- rather
#       than file-based data transfer

# For Globus_io connections - need to set port to that reported
# by library when application launched
setenv REG_CONNECTOR_HOSTNAME bezier.man.ac.uk
setenv REG_CONNECTOR_PORT 4478

#---------------------------------------------------------------
#       This section only necessary if using the UNICORE 
#       framework

# Handle of registry to bind to
setenv REGISTRY_GSH http://localhost:8888/Unicore/Registry

# Location of ReG grid service
setenv REG_GS ${HOME}/projects/ReG-grid-service

# Additions to java classpath for grid service and steering proxy
# Assumes classpath already partially set-up for GLUE package from
# The Mind Electric.
setenv CLASSPATH ${CLASSPATH}:${HOME}/bin
setenv CLASSPATH ${CLASSPATH}:${REG_GS}
setenv CLASSPATH ${CLASSPATH}:${REG_GS}/ajo.jar

#---------------------------------------------------------------
