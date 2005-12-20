#  Makefile for utilities intended to aim investigation of 
#  correspondence of C types to F90 kind parameters on any given 
#  architecture.
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
#--------------------------------------------------------------------

include ../../Makefile.include
include ../../make/Makefile.${ARCH}

all:
	make sizeof_f.o
	make max_kind
	make c_types

sizeof_f.o: sizeof_f.m4
	${M4} ${M4FILE} sizeof_f.m4 > sizeof_f.c
	${CC} ${C_FLAGS} -c sizeof_f.c -o sizeof_f.o

max_kind: max_kind.f90 sizeof_f.o
	${F90} ${F90_FLAGS} -c max_kind.f90 -o max_kind.o
	${F90} ${F90_FLAGS} max_kind.o sizeof_f.o -o max_kind

c_types: c_types.c
	${CC} ${C_FLAGS} ${EXTRA_C_FLAGS} c_types.c -o c_types

clean:
	rm -f *~ *.o max_kind c_types sizeof_f.c
 
