#-----------------------------------------------------------------------
#    Makefile for miniature parallel F90 steerable application.
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
#    Initial version by:  Andrew Porter.
#-----------------------------------------------------------------------
#

include ../../Makefile.include

TARGET   = mini_app_para

.f90.o:
	${F90} ${F90_FLAGS} ${STEER_INCLUDES} ${GLOBUS_INCLUDES} -c $< -o $@

${TARGET}: ${TARGET}.o ../../include/reg_steer_f90.inc
	${F90} ${F90_FLAGS} ${TARGET}.o -o $@ ${STEER_LIBS} ${GLOBUS_LDFLAGS} \
	-lmpi -lReG_Steer -lm -lz -lxml2 ${GLOBUS_PKG_LIBS} 
	mv ${TARGET} ${HOME}/bin/.

mini_app_para.o: mini_app_para.f90
	${F90} ${F90_FLAGS} ${STEER_INCLUDES} ${GLOBUS_INCLUDES} -c $< -o $@

clean:
	rm -f *.o *~ core ${TARGET}

