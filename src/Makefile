#    Makefile for RealityGrid steering library.
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
#    Initial version by:  A Porter, 23.7.2002
#
#------------------------------------------------------------------------

include ../Makefile.include

# Set-up access to M4 scripts for production of F90 wrappers, this is
# taken from the PVM project.
M4FILE = conf/${ARCH}.m4

INCLUDE_PATH   = ${STEER_INCLUDES} ${GLOBUS_INCLUDES} ${SOAP_INCLUDES}
LIB_PATH       = ${REG_STEER_HOME}/lib$(NBIT)

all:
	make ${LIB_PATH}/libReG_Steer.a
	make ${LOCAL_BIN_DIR}/ReG_Steer_Proxy.class

lib:
	make ${LIB_PATH}/libReG_Steer.a

proxy:
	make ${LOCAL_BIN_DIR}/ReG_Steer_Proxy.class

.c.o:
	${CC} ${C_FLAGS} -DREG_GLOBUS_SAMPLES=${REG_GLOBUS_SAMPLES_FLAG} \
	-DREG_GLOBUS_STEERING=${REG_GLOBUS_STEERING_FLAG} \
        -DREG_SOAP_STEERING=${REG_SOAP_STEERING_FLAG} \
        ${INCLUDE_PATH} -c $< -o $@

${LIB_PATH}/libReG_Steer.a: ReG_Steer_Appside_f.o \
	ReG_Steer_Appside.o \
	ReG_Steer_Steerside.o \
	ReG_Steer_Common.o \
	ReG_Steer_Proxy_utils.o \
	ReG_Steer_XML.o \
	ReG_Steer_Globus_io.o \
	ReG_Steer_Steerside_Globus.o \
	ReG_Steer_Appside_Globus.o \
        ReG_Steer_Steerside_Soap.o \
        ReG_Steer_Appside_Soap.o \
	soapC.o soapClient.o


${LIB_PATH}/libReG_Steer.a:
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_f.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Common.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Proxy_utils.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_XML.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Globus_io.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_Globus.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside_Globus.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_Soap.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside_Soap.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a soapC.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a soapClient.o


${LOCAL_BIN_DIR}/ReG_Steer_Proxy.class:
	   ${JAVAC} ReG_Steer_Proxy.java
	   mv -f ReG_Steer_Proxy.class ${BIN_DIR}/.

#---------------------------------------------------------------

ReG_Steer_Appside.o: ReG_Steer_Appside.c \
	 ../include/ReG_Steer_Appside.h \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Appside_internal.h \
	 ../include/ReG_Steer_Common.h \
	 ../include/ReG_Steer_Globus_io.h \
	 ../include/ReG_Steer_Appside_Globus.h \
         ../include/ReG_Steer_Appside_Soap.h

ReG_Steer_Steerside.o: ReG_Steer_Steerside.c \
         ReG_Steer_Proxy.java \
         ReG_Steer_Proxy_utils.c \
	 ReG_Steer_XML.c \
	 ../include/ReG_Steer_XML.h \
	 ../include/ReG_Steer_Proxy_utils.h \
	 ../include/ReG_Steer_Steerside.h \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Steerside_internal.h \
	 ../include/ReG_Steer_Common.h \
	 ../include/ReG_Steer_Globus_io.h \
	 ../include/ReG_Steer_Steerside_Globus.h \
	 ../include/ReG_Steer_Steerside_Soap.h

ReG_Steer_Common.o: ReG_Steer_Common.c \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Common.h

ReG_Steer_Proxy_utils.o: ReG_Steer_Proxy_utils.c \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Proxy_utils.h

ReG_Steer_Appside_f.c: ReG_Steer_Appside_f.m4 \
	 ${M4FILE}
	${M4} ${M4FILE} ReG_Steer_Appside_f.m4 > $@

ReG_Steer_Appside_f.o: ReG_Steer_Appside_f.c \
         ../include/ReG_Steer_Appside_internal.h \
         ../include/ReG_Steer_Appside.h \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Common.h \
	 ../include/ReG_Steer_Globus_io.h \
	 ../include/ReG_Steer_Appside_Globus.h

ReG_Steer_XML.o: ReG_Steer_XML.c \
	../include/ReG_Steer_XML.h \
	../include/ReG_Steer_types.h

ReG_Steer_Globus_io.o: ReG_Steer_Globus_io.c \
	../include/ReG_Steer_Globus_io.h \
	../include/ReG_Steer_types.h

ReG_Steer_Appside_Globus.o: ReG_Steer_Appside_Globus.c \
	../include/ReG_Steer_Globus_io.h
	../include/ReG_Steer_Appside_Globus.h \
	../include/ReG_Steer_Appside_internal.h \
	../include/ReG_Steer_types.h \
	../include/ReG_Steer_Common.h

ReG_Steer_Steerside_Globus.o: ReG_Steer_Steerside_Globus.c \
	../include/ReG_Steer_Steerside_Globus.h \
        ../include/ReG_Steer_Steerside.h \
	../include/ReG_Steer_Steerside_internal.h

ReG_Steer_Appside_Soap.o: ReG_Steer_Appside_Soap.c \
        ../include/ReG_Steer_Appside_Soap.h \
        ../include/ReG_Steer_Appside_internal.h \
	../include/ReG_Steer_types.h \
        ../include/soapSGS.nsmap \
	soapC.c soapClient.c ../include/soapH.h

ReG_Steer_Steerside_Soap.o: ReG_Steer_Steerside_Soap.c \
        ../include/ReG_Steer_Steerside_Soap.h \
        ../include/ReG_Steer_Steerside_internal.h \
        ../include/ReG_Steer_Steerside.h \
	../include/ReG_Steer_types.h \
        ../include/soapSGS.nsmap \
	soapC.c soapClient.c ../include/soapH.h

soapC.c: ../SGS/SGS.wsdl
	cd ../SGS; make

soapC.o: soapC.c \
         ../include/soapH.h

soapClient.c: ../SGS/SGS.wsdl
	cd ../SGS; make

soapClient.o: soapClient.c \
         ../include/soapH.h

../include/soapH.h: ../SGS/SGS.wsdl
	cd ../SGS; make

../include/soapSGS.nsmap: ../SGS/SGS.wsdl
	cd ../SGS; make

../include/soapStub.h: ../SGS/SGS.wsdl
	cd ../SGS; make

#---------------------------------------------------------------

clean:
	rm -f *.o ReG_Steer.ln *~ 
	rm -f ReG_Steer_Appside_f.c
	rm -f soapC.c soapClient.c
	rm -f ../include/*~
	rm -f ${LIB_PATH}/libReG_Steer.a

tidy:
	rm -f ReG_Steer_Steerside.o ReG_Steer_Appside.o \
	ReG_Steer_Common.o ReG_Steer_Appside_f.o ReG_Steer_Appside_f.c *~

