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

# Edit the two following lines if 32-bit binaries are desired 
# (NBIT = 32 and G=-n32).
NBIT         = 32
GLOBAL_FLAGS = G="-n32 -g" NBIT=$(NBIT)
LOCAL_BIN_DIR= ${HOME}/bin

all:
	make steerer
	make mini_vis
	make app
	make app_f90
	make app_f90_parallel

lib$(NBIT)/libReG_Steer.a: include/*.h \
 src/*.c \
 src/*.m4 \
 src/*.java
	cd src; make ${GLOBAL_FLAGS}

mini_vis: lib$(NBIT)/libReG_Steer.a \
 examples/mini_vis/*.c
	cd examples/mini_vis; make ${GLOBAL_FLAGS}

app: lib$(NBIT)/libReG_Steer.a \
 examples/mini_app/*.c
	cd examples/mini_app; make ${GLOBAL_FLAGS}

app_f90: lib$(NBIT)/libReG_Steer.a \
 include/*.inc \
 examples/mini_app_f90/*.f90
	cd examples/mini_app_f90; make ${GLOBAL_FLAGS}

app_f90_parallel:  lib$(NBIT)/libReG_Steer.a \
 include/*.inc \
 examples/mini_app_f90_parallel/*.f90
	cd examples/mini_app_f90_parallel; make ${GLOBAL_FLAGS}

steerer: lib$(NBIT)/libReG_Steer.a \
 ${LOCAL_BIN_DIR}/ReG_Steer_Proxy.class \
 examples/mini_steerer/*.c
	cd examples/mini_steerer; make ${GLOBAL_FLAGS}

${LOCAL_BIN_DIR}/ReG_Steer_Proxy.class: src/*.java
	cd src; make  ${GLOBAL_FLAGS}

clean:
	cd src; make ${GLOBAL_FLAGS} clean 
	cd examples/mini_vis; make ${GLOBAL_FLAGS} clean
	cd examples/mini_app; make ${GLOBAL_FLAGS} clean
	cd examples/mini_app_f90; make ${GLOBAL_FLAGS} clean
	cd examples/mini_app_f90_parallel; make ${GLOBAL_FLAGS} clean
	cd examples/mini_steerer; make ${GLOBAL_FLAGS} clean

tar:
	make clean
	cd ..; tar -cf reg_steer_backup.tar reg_steer_lib/*; \
gzip reg_steer_backup.tar
