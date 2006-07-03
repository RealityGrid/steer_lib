#  Top-level makefile for the RealityGrid steering library and
#  associated example applications.
#
#  (C) Copyright 2006, University of Manchester, United Kingdom,
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
	$(MAKE) lib
	$(MAKE) lib_utils
	$(MAKE) c_examples
	$(MAKE) f90_examples
	$(MAKE) wrappers

no_f90:
	$(MAKE) lib
	$(MAKE) lib_utils
	$(MAKE) c_examples
	$(MAKE) wrappers

c_examples:
	$(MAKE) simple
	$(MAKE) app
	$(MAKE) steerer
	$(MAKE) sink

f90_examples:
	$(MAKE) app_f90
	$(MAKE) app_f90_parallel

lib: 
	cd src; $(MAKE) $(LIB_PATH)/$(LIB_NAME)

lib_utils: 
	cd src; $(MAKE) $(LIB_PATH)/$(LIB_UTILS_NAME)

libss:
	cd src; $(MAKE) -f Makefile.ss all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapSockets.so $(LIB_PATH)/libReG_Steer.so

libssd:
	cd src; $(MAKE) -f Makefile.ssd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapSocketsDebug.so $(LIB_PATH)/libReG_Steer.so

libsf:
	cd src; $(MAKE) -f Makefile.sf all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapFile.so $(LIB_PATH)/libReG_Steer.so

libsfd:
	cd src; $(MAKE) -f Makefile.sfd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerSoapFileDebug.so $(LIB_PATH)/libReG_Steer.so

libfs:
	cd src; $(MAKE) -f Makefile.fs all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFileSockets.so $(LIB_PATH)/libReG_Steer.so

libfsd:
	cd src; $(MAKE) -f Makefile.fsd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFileSocketsDebug.so $(LIB_PATH)/libReG_Steer.so

libf:
	cd src; $(MAKE) -f Makefile.f all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFile.so $(LIB_PATH)/libReG_Steer.so

libfd:
	cd src; $(MAKE) -f Makefile.fd all
	rm -f $(LIB_PATH)/libReG_Steer.so
	ln -s libReG_SteerFileDebug.so $(LIB_PATH)/libReG_Steer.so

wrappers: $(LIB_PATH)/$(LIB_NAME)
	cd wrappers; $(MAKE)

simple: $(LIB_PATH)/$(LIB_NAME)
	cd examples/simple; $(MAKE)

app: $(LIB_PATH)/$(LIB_NAME)
	cd examples/mini_app; $(MAKE)


app_f90: $(LIB_PATH)/$(LIB_NAME)
	cd examples/mini_app_f90; $(MAKE)

app_f90_parallel:  $(LIB_PATH)/$(LIB_NAME)
	cd examples/mini_app_f90_parallel; $(MAKE)

steerer: $(LIB_PATH)/$(LIB_NAME)
	cd examples/mini_steerer; $(MAKE)

sink: $(LIB_PATH)/$(LIB_NAME)
	cd examples/sink; $(MAKE)

install:
	cd examples/simple; $(MAKE) install
	cd examples/mini_app; $(MAKE) install
	cd examples/mini_steerer; $(MAKE) install
	cd examples/mini_app_f90; $(MAKE) install
	cd examples/mini_app_f90_parallel; $(MAKE) install
	cd examples/sink; $(MAKE) install

clean:
	cd src; $(MAKE) clean 
	cd examples/simple; $(MAKE) clean
	cd examples/mini_app; $(MAKE) clean
	cd examples/mini_steerer; $(MAKE) clean
	cd examples/mini_app_f90; $(MAKE) clean
	cd examples/mini_app_f90_parallel; $(MAKE) clean
	cd examples/sink; $(MAKE) clean
	cd wrappers; $(MAKE) clean

doc:
	doxygen docs/Doxyfile
	cd docs/html; cp -f ../steer_arch_symm.png .

tar:
	$(MAKE) clean
	cd ..; tar -cf reg_steer_backup.tar reg_steer_lib/*
	gzip reg_steer_backup.tar
