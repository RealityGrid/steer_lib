#    Makefile for utilities intended to aim investigation of 
#    correspondence of C types to F90 kind parameters on any given 
#    architecture.
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
#    Initial version by:  A Porter.
#
#--------------------------------------------------------------------
F90_FLAGS = 
CC_FLAGS  = 

# Set-up access to M4 scripts for production of F90 wrappers, this is
# taken from the PVM project.
ARCH           = SGI
M4             = m4
M4FILE         = ${REG_STEER_HOME}/src/conf/${ARCH}.m4

all:
	make sizeof_f
	make max_kind
	make c_types

sizeof_f: sizeof_f.m4
	${M4} ${M4FILE} sizeof_f.m4 > sizeof_f.c
	cc ${CC_FLAGS} -c sizeof_f.c -o sizeof_f.o

max_kind: max_kind.f90 sizeof_f.m4
	f90 ${F90_FLAGS} -c max_kind.f90 -o max_kind.o
	f90 ${F90_FLAGS} max_kind.o sizeof_f.o -o max_kind

c_types: c_types.c
	cc ${CC_FLAGS} c_types.c -o c_types

clean:
	rm -f *~ *.o max_kind c_types sizeof_f.c
 
