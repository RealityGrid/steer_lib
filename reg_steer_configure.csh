# File to set up environment for RealityGrid steering
# Andrew Porter, 25.10.2002.

# Set root directory for library installation
setenv REG_STEER_HOME $HOME/projects/reg_steer_lib

# Set directory location to use for steering communication
setenv REG_STEER_DIRECTORY /scratch/zzcguap/steering/

# Set location of XML parser library and header files
setenv REG_XML_LIBDIR ${HOME}/projects/xml_parsing/libxml2-2.4.24/lib
setenv REG_XML_INCDIR ${HOME}/projects/xml_parsing/libxml2-2.4.24/include

# Handle of registry to bind to
setenv REGISTRY_GSH http://localhost:8888/Unicore/Registry

# Location of ReG grid service
setenv REG_GS ${HOME}/projects/ReG-grid-service

# Additions to java classpath for grid service and steering proxy
setenv CLASSPATH ${CLASSPATH}:${HOME}/bin
setenv CLASSPATH ${CLASSPATH}:${REG_GS}
setenv CLASSPATH ${CLASSPATH}:${REG_GS}/ajo.jar

