#  Makefile for RealityGrid steering library.
#
#  (C) Copyright 2005, University of Manchester, United Kingdom,
#  all rights reserved.
#
#  This software was developed by the RealityGrid project
#  (http://www.realitygrid.org), funded by the EPSRC under grants
#  GR/R67699/01 and GR/R67699/02.
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
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
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
include Makefile.shared

INCLUDE_PATH   = ${STEER_INCLUDES}
LIB_PATH       = ${REG_STEER_HOME}/lib$(NBIT)

.SUFFIXES: .o .f90 .c

REG_LIB_OBJECTS=ReG_Steer_Appside_f.o \
 ReG_Steer_Appside.o \
 ReG_Steer_Steerside.o \
 ReG_Steer_Common.o \
 ReG_Steer_Proxy_utils.o \
 ReG_Steer_XML.o \
 ReG_Steer_Appside_Sockets.o \
 ReG_Steer_Steerside_Soap.o \
 ReG_Steer_Appside_Soap.o \
 ReG_Steer_Appside_File.o \
 ReG_Steer_Logging.o \
 stdsoap2.o soapC.o soapClient.o \
 Base64.o \
 ReG_Steering_Initialize.o \
 ReG_Steer_Steerside_WSRF.o \
 ReG_Steer_Appside_WSRF.o

REG_LIB_UTILS_OBJECTS=ReG_Steer_Utils_WSRF.o \
 ReG_Steer_Utils.o \
 ReG_Steer_Browser.o

.c.o:
	${CC} ${C_FLAGS} \
        -DREG_SOCKET_SAMPLES=${REG_SOCKET_SAMPLES_FLAG} \
        -DREG_SOAP_STEERING=${REG_SOAP_STEERING_FLAG} \
        -DREG_DEBUG=${REG_DEBUG_FLAG} \
        -DREG_DEBUG_FULL=${REG_DEBUG_FULL_FLAG} \
        -DREG_STEER_LIB_VERSION=\"${REG_STEER_LIB_VERSION}\" \
        ${INCLUDE_PATH} -c $< -o $@

all:
	$(MAKE) ${LIB_PATH}/$(LIB_NAME)
	$(MAKE) ${LIB_PATH}/libReG_Steer_Utils.a

libutils:
	$(MAKE) ${LIB_PATH}/$(LIB_UTILS_NAME)

lib:
	echo $(MAKE) ${LIB_PATH}/$(LIB_NAME)
	$(MAKE) ${LIB_PATH}/$(LIB_NAME)

${LIB_PATH}/libReG_Steer.so: ${REG_LIB_OBJECTS}
	$(MAKE) -f Makefile.ss all
	$(MAKE) -f Makefile.ssd all
	$(MAKE) -f Makefile.sf all
	$(MAKE) -f Makefile.sfd all
	$(MAKE) -f Makefile.fs all
	$(MAKE) -f Makefile.fsd all
	$(MAKE) -f Makefile.ff all
	$(MAKE) -f Makefile.ffd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapSockets.so $(LIB_PATH)/libReG_Steer.so

${LIB_PATH}/libReG_Steer.a: ${REG_LIB_OBJECTS}
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
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Logging.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a stdsoap2.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a soapC.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a soapClient.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a Base64.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside_WSRF.o
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_WSRF.o
# '-' is to allow for failure in case we don't have an F90 compiler
	-${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer.a ReG_Steering_Initialize.o

${LIB_PATH}/libReG_Steer_Utils.a: ${REG_LIB_UTILS_OBJECTS}
	${AR} ${AR_FLAGS} ${LIB_PATH}/libReG_Steer_Utils.a ${REG_LIB_UTILS_OBJECTS}

#---------------------------------------------------------------

ReG_Steer_Appside.o: ReG_Steer_Appside.c \
	 ../include/ReG_Steer_Appside.h \
	 ../include/ReG_Steer_types.h \
	 ../include/ReG_Steer_Appside_internal.h \
	 ../include/ReG_Steer_Common.h \
	 ../include/ReG_Steer_Appside_Sockets.h \
         ../include/ReG_Steer_Appside_Soap.h \
         ../include/ReG_Steer_Appside_File.h \
         ../include/ReG_Steer_Logging.h

ReG_Steer_Steerside.o: ReG_Steer_Steerside.c \
         ReG_Steer_Proxy_utils.c \
	 ReG_Steer_XML.c \
         ReG_Steer_Browser.c \
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
        ../include/soapRealityGrid.nsmap \
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
        ../include/soapRealityGrid.nsmap \
        ../include/soapH.h \
	stdsoap2.c soapC.c soapClient.c 

ReG_Steer_Steerside_WSRF.o: ReG_Steer_Steerside_WSRF.c \
        ../include/ReG_Steer_XML.h \
        ../include/ReG_Steer_Steerside_internal.h \
        ../include/ReG_Steer_Steerside.h \
        ../include/soapRealityGrid.nsmap \
        ../include/soapH.h \
	stdsoap2.c soapC.c soapClient.c 

ReG_Steer_Logging.o: ReG_Steer_Logging.c \
        ../include/ReG_Steer_Logging.h \
        ../include/ReG_Steer_types.h \
        ../include/ReG_Steer_Appside_internal.h

Base64.o: Base64.c \
        ../include/Base64.h

# Leading '-' is to allow for failure in case we don't have an 
# F90 compiler
ReG_Steering_Initialize.o: ReG_Steering_Initialize.f90 \
        ../include/reg_steer_f90.inc
	-${F90} ${F90_FLAGS} \
        -DREG_SOCKET_SAMPLES=${REG_SOCKET_SAMPLES_FLAG} \
        -DREG_SOAP_STEERING=${REG_SOAP_STEERING_FLAG} \
        -DREG_DEBUG=${REG_DEBUG_FLAG} \
        -DREG_DEBUG_FULL=${REG_DEBUG_FULL_FLAG} \
        -DREG_STEER_LIB_VERSION=\"${REG_STEER_LIB_VERSION}\" \
        ${INCLUDE_PATH} -c $< -o $@

ReG_Steer_Browser.o: ReG_Steer_Browser.c \
        ../include/ReG_Steer_Browser.h \
	../include/ReG_Steer_types.h \
        ../include/ReG_Steer_XML.h \
        ../include/soapRealityGrid.nsmap \
        ../include/soapH.h \
        ../include/soapStub.h \
	stdsoap2.c soapC.c soapClient.c

ReG_Steer_Utils_WSRF.o: ReG_Steer_Utils_WSRF.c \
           ../include/ReG_Steer_types.h \
           ../include/ReG_Steer_Utils_WSRF.h \
           ../include/ReG_Steer_Common.h \
           ../include/ReG_Steer_XML.h \
           ../include/soapH.h

ReG_Steer_Utils.o: ReG_Steer_Utils.c \
           ../include/ReG_Steer_types.h \
           ../include/ReG_Steer_Utils.h \
           ../include/soapH.h

#---------------------------------------------------------------

clean:
	rm -f *.o ReG_Steer.ln *~ 
	rm -f ReG_Steer_Appside_f.c
	rm -f ../include/*~
	rm -f ${LIB_PATH}/libReG_Steer.a
	rm -f ${LIB_PATH}/libReG_Steer*.so
	rm -f ${LIB_PATH}/libReG_Steer_Utils.a

tidy:
	rm -f ReG_Steer_Steerside.o ReG_Steer_Appside.o \
        ReG_Steer_Appside_Sockets.o ReG_Steer_Appside_Soap.o \
        ReG_Steer_Appside_File.o ReG_Steer_Browser.o \
	ReG_Steer_Common.o ReG_Steer_Appside_f.o \
        ReG_Steer_Logging.o ReG_Steer_Appside_f.c *~
