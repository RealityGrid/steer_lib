#---------------------------------------------------------------------
#    Makefile for mini-application
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
#    Initial version by:   A Porter
#---------------------------------------------------------------------
#

include ../../globus_definitions

# G and NBIT are passed to make on cmd line

TARGET   = mini_app
CC       = cc
CC_FLAGS = -ansi $(G) -I${REG_STEER_HOME}/include ${GLOBUS_INCLUDES}

# STEER_LIBS is the directory where libReG_Steer.a can be found
STEER_LIBS = -L${REG_STEER_HOME}/lib$(NBIT)
XML_LIBS   = -L${REG_XML_LIBDIR}$(NBIT)

.c.o:
	${CC} ${CC_FLAGS} -c $< -o $@

${TARGET}: ${TARGET}.o
	${CC} ${CC_FLAGS} ${TARGET}.o -o $@ \
${XML_LIBS} ${STEER_LIBS} ${GLOBUS_LDFLAGS} -lReG_Steer -lxml2 -lz -lm ${GLOBUS_PKG_LIBS}
	mv ${TARGET} ${HOME}/bin/.

clean:
	rm -f *.o *~ core ${TARGET}

