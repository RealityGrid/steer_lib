#-----------------------------------------------------------------
#    Makefile for miniature, serial steerable-application in F90
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
#    Initial version by: <name>
#-----------------------------------------------------------------

include ../../Makefile.include

# G and NBIT are passed to make on cmd line

TARGET   = mini_app2

.f90.o:
	${F90} ${F90_FLAGS} -C ${STEER_INCLUDES} ${GLOBUS_INCLUDES} -c $< -o $@

${TARGET}: ${TARGET}.o ${REG_STEER_HOME}/include/reg_steer_f90.inc
	${F90} ${F90_FLAGS} ${TARGET}.o -o $@ ${STEER_LIBS} ${GLOBUS_LDFLAGS} \
	${SOAP_LDFLAGS} -lReG_Steer -lz -lxml2 ${GLOBUS_PKG_LIBS} \
	${SOAP_PKG_LIBS}
	mv ${TARGET} ${LOCAL_BIN_DIR}/.

mini_app2.o: mini_app2.f90
	${F90} ${F90_FLAGS} -C ${STEER_INCLUDES} ${GLOBUS_INCLUDES} -c $< -o $@

clean:
	rm -f *.o *~ core ${TARGET}

