#  Top-level makefile for the RealityGrid steering library and
#  associated example applications.
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
#----------------------------------------------------------------------

include Makefile.include
include make/Makefile.${ARCH}

LIB_PATH       = ${REG_STEER_HOME}/lib$(NBIT)

all:
	make $(LIB_PATH)/$(LIB_NAME)
	make c_examples
	make f90_examples

no_f90:
	make $(LIB_PATH)/$(LIB_NAME)
	make c_examples

c_examples:
	make app
	make steerer
	make sink

f90_examples:
	make app_f90
	make app_f90_parallel

$(LIB_PATH)/$(LIB_NAME): include/*.h \
 src/*.c \
 src/*.m4
	rm -f $(LIB_PATH)/$(LIB_NAME)
	cd src; make lib

libss: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.ss all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapSockets.so $(LIB_PATH)/libReG_Steer.so

libssd: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.ssd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapSocketsDebug.so $(LIB_PATH)/libReG_Steer.so

libsf: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.sf all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapFile.so $(LIB_PATH)/libReG_Steer.so

libsfd: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.sfd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapFileDebug.so $(LIB_PATH)/libReG_Steer.so

libfs: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.fs all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFileSockets.so $(LIB_PATH)/libReG_Steer.so

libfsd: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.fsd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFileSocketsDebug.so $(LIB_PATH)/libReG_Steer.so

libf: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.f all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFile.so $(LIB_PATH)/libReG_Steer.so

libfd: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make -f Makefile.fd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFileDebug.so $(LIB_PATH)/libReG_Steer.so

app: $(LIB_PATH)/$(LIB_NAME) \
 examples/mini_app/*.c
	cd examples/mini_app; make


app_f90: $(LIB_PATH)/$(LIB_NAME) \
 include/*.inc \
 examples/mini_app_f90/*.f90
	cd examples/mini_app_f90; make

app_f90_parallel:  $(LIB_PATH)/$(LIB_NAME) \
 include/*.inc \
 examples/mini_app_f90_parallel/*.f90
	cd examples/mini_app_f90_parallel; make

steerer: $(LIB_PATH)/$(LIB_NAME) \
 examples/mini_steerer/*.c
	cd examples/mini_steerer; make

sink: $(LIB_PATH)/$(LIB_NAME) \
 examples/mini_steerer/*.c
	cd examples/sink; make

install:
	cd examples/mini_app; make install
	cd examples/mini_steerer; make install
	cd examples/mini_app_f90; make install
	cd examples/mini_app_f90_parallel; make install
	cd examples/sink; make install

clean:
	cd src; make clean 
	cd examples/mini_app; make clean
	cd examples/mini_steerer; make clean
	cd examples/mini_app_f90; make clean
	cd examples/mini_app_f90_parallel; make clean
	cd examples/sink; make clean

tar:
	make clean
	cd ..; tar -cf reg_steer_backup.tar reg_steer_lib/*; \
gzip reg_steer_backup.tar
