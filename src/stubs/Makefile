#  Makefile for null RealityGrid steering library.
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

include ../../Makefile.include
include ../../make/Makefile.${ARCH}

INCLUDE_PATH   = ${STEER_INCLUDES}
LIB_PATH       = ${REG_STEER_HOME}/lib$(NBIT)

.c.o:
	${CC} ${C_FLAGS} \
        -DREG_SOCKET_SAMPLES=${REG_SOCKET_SAMPLES_FLAG} \
        -DREG_SOAP_STEERING=${REG_SOAP_STEERING_FLAG} \
        -DREG_DEBUG=${REG_DEBUG_FLAG} \
        -DREG_DEBUG_FULL=${REG_DEBUG_FULL_FLAG} \
        -DREG_STEER_LIB_VERSION=\"${REG_STEER_LIB_VERSION}\" \
        ${INCLUDE_PATH} -c $< -o $@

all: ReG_Steering_Initialize_stub.o
	cp -f ReG_Steering_Initialize_stub.o ../ReG_Steering_Initialize.o


#---------------------------------------------------------------

clean: tidy

tidy:
	rm -f *.o *~
