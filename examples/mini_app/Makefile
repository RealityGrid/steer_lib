#---------------------------------------------------------------------
#  Makefile for mini-application
#
#  (C) Copyright 2006, University of Manchester, United Kingdom,
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
#---------------------------------------------------------------------
#

include ${REG_STEER_HOME}/Makefile.include
include ${REG_STEER_HOME}/make/Makefile.${ARCH}

TARGET   = mini_app

.c.o:
	${CC} ${C_FLAGS} ${STEER_INCLUDES} ${GLOBUS_INCLUDES} -c $< -o $@

${TARGET}: ${REG_STEER_HOME}/lib$(NBIT)/$(LIB_NAME) ${TARGET}.o
	${CC} ${C_FLAGS} ${TARGET}.o -o $@ ${STEER_LIBS} \
        -Wl,-Bstatic -lReG_Steer -lReG_Steer_Utils -lReG_Steer_SOAP \
	-Wl,-Bdynamic ${STEER_LDFLAGS}

install:
	-mv -f ${TARGET} ${LOCAL_BIN_DIR}/.

clean:
	rm -f *.o *~ core ${TARGET}

