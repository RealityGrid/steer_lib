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

all:
	make lib
	make c_examples
	make f90_examples

no_f90:
	make lib
	make c_examples

c_examples:
	make app
	make steerer
	make sink

f90_examples:
	make app_f90
	make app_f90_parallel

lib: include/*.h \
 src/*.c \
 src/*.m4
	cd src; make lib

app: lib$(NBIT)/libReG_Steer.a \
 examples/mini_app/*.c
	cd examples/mini_app; make


app_f90: lib$(NBIT)/libReG_Steer.a \
 include/*.inc \
 examples/mini_app_f90/*.f90
	cd examples/mini_app_f90; make

app_f90_parallel:  lib$(NBIT)/libReG_Steer.a \
 include/*.inc \
 examples/mini_app_f90_parallel/*.f90
	cd examples/mini_app_f90_parallel; make

steerer: lib$(NBIT)/libReG_Steer.a \
 examples/mini_steerer/*.c
	cd examples/mini_steerer; make

sink: lib$(NBIT)/libReG_Steer.a \
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
