#    Top-level makefile for the RealityGrid steering library and
#    associated example applications.
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
#    Initial version by: A Porter
#----------------------------------------------------------------------

include Makefile.include

all:
	make steerer
	make c_examples
	make f90_examples

no_f90:
	make steerer
	make c_examples

c_examples:
	make app
	make steerer

f90_examples:
	make app_f90
	make app_f90_parallel

lib$(NBIT)/libReG_Steer.a: include/*.h \
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

clean:
	cd src; make clean 
	cd examples/mini_app; make clean
	cd examples/mini_steerer; make clean
	cd examples/mini_app_f90; make clean
	cd examples/mini_app_f90_parallel; make clean

tar:
	make clean
	cd ..; tar -cf reg_steer_backup.tar reg_steer_lib/*; \
gzip reg_steer_backup.tar
