#  Makefile for RealityGrid steering library.
#
#  (C) Copyright 2002, 2004, University of Manchester, United Kingdom,
#  all rights reserved.
#
#  This software is produced by the Supercomputing, Visualization and
#  e-Science Group, Manchester Computing, University of Manchester
#  as part of the RealityGrid project (http://www.realitygrid.org),
#  funded by the EPSRC under grants GR/R67699/01 and GR/R67699/02.
#
#  LICENCE TERMS
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#
#  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
#  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
#  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
#  CORRECTION.
#
#  Authors........: Andrew Porter, Robert Haines
#
#------------------------------------------------------------------------

include ../Makefile.include
include ../make/Makefile.${ARCH}

# Set-up access to M4 scripts for production of F90 wrappers, this is
# taken from the PVM project.
M4FILE = conf/${ARCH}.m4

INCLUDE_PATH   = ${STEER_INCLUDES}
LIB_PATH       = ${REG_STEER_HOME}/lib$(NBIT)

all:
	make ${LIB_PATH}/libReG_Steer.a

lib:
	make ${LIB_PATH}/libReG_Steer.a

.c.o:
	${CC} ${C_FLAGS} \
        -DREG_SOCKET_SAMPLES=${REG_SOCKET_SAMPLES_FLAG} \
        -DREG_SOAP_STEERING=${REG_SOAP_STEERING_FLAG} \
        -DREG_DEBUG=${REG_DEBUG_FLAG} \
        -DREG_STEER_LIB_VERSION=\"${REG_STEER_LIB_VERSION}\" \
        ${INCLUDE_PATH} -c $< -o $@

${LIB_PATH}/libReG_Steer.a: ReG_Steer_Appside_f.o \
	ReG_Steer_Appside.o \
	ReG_Steer_Steerside.o \
	ReG_Steer_Common.o \
	ReG_Steer_Proxy_utils.o \
	ReG_Steer_XML.o \
	ReG_Steer_Appside_Sockets.o \
        ReG_Steer_Steerside_Soap.o \
        ReG_Steer_Appside_Soap.o \
        ReG_Steer_Appside_File.o \
	stdsoap2.o soapC.o soapClient.o


${LIB_PATH}/libReG_Steer.a:
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_f.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Common.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Proxy_utils.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_XML.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_Sockets.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_Soap.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_File.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside_Soap.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a stdsoap2.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a soapC.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a soapClient.o

#---------------------------------------------------------------

ReG_Steer_Appside.o: ReG_Steer_Appside.c \
	 ../include/ReG_Steer_Appside.h \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Appside_internal.h \
	 ../include/ReG_Steer_Common.h \
	 ../include/ReG_Steer_Appside_Sockets.h \
         ../include/ReG_Steer_Appside_Soap.h \
         ../include/ReG_Steer_Appside_File.h

ReG_Steer_Steerside.o: ReG_Steer_Steerside.c \
         ReG_Steer_Proxy_utils.c \
	 ReG_Steer_XML.c \
	 ../include/ReG_Steer_XML.h \
	 ../include/ReG_Steer_Proxy_utils.h \
	 ../include/ReG_Steer_Steerside.h \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Steerside_internal.h \
	 ../include/ReG_Steer_Common.h \
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
	 ../include/ReG_Steer_Appside_Sockets.h

ReG_Steer_XML.o: ReG_Steer_XML.c \
	../include/ReG_Steer_XML.h \
	../include/ReG_Steer_types.h

ReG_Steer_Appside_Sockets.o: ReG_Steer_Appside_Sockets.c \
	../include/ReG_Steer_Appside_Sockets.h \
	../include/ReG_Steer_types.h

ReG_Steer_Appside_Soap.o: ReG_Steer_Appside_Soap.c \
        ../include/ReG_Steer_Appside_Soap.h \
        ../include/ReG_Steer_Appside_internal.h \
	../include/ReG_Steer_types.h \
        ../include/soapSGS.nsmap \
        ../include/soapH.h \
	stdsoap2.c soapC.c soapClient.c

ReG_Steer_Appside_File.o: ReG_Steer_Appside_File.c \
        ../include/ReG_Steer_Appside_File.h \
        ../include/ReG_Steer_Appside_internal.h \
	../include/ReG_Steer_types.h

ReG_Steer_Steerside_Soap.o: ReG_Steer_Steerside_Soap.c \
        ../include/ReG_Steer_Steerside_Soap.h \
        ../include/ReG_Steer_Steerside_internal.h \
        ../include/ReG_Steer_Steerside.h \
	../include/ReG_Steer_types.h \
        ../include/soapSGS.nsmap \
        ../include/soapH.h \
	stdsoap2.c soapC.c soapClient.c 

#---------------------------------------------------------------

clean:
	rm -f *.o ReG_Steer.ln *~ 
	rm -f ReG_Steer_Appside_f.c
	rm -f ../include/*~
	rm -f ${LIB_PATH}/libReG_Steer.a

tidy:
	rm -f ReG_Steer_Steerside.o ReG_Steer_Appside.o \
        ReG_Steer_Appside_Sockets.o ReG_Steer_Appside_Soap.o \
        ReG_Steer_Appside_File.o \
	ReG_Steer_Common.o ReG_Steer_Appside_f.o ReG_Steer_Appside_f.c *~
