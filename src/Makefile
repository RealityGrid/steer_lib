#    Makefile for 64-bit steering library.
#    Currently only tested on SGI Origin.
#
#    (C)Copyright 2002 The University of Manchester, United Kingdom,
#    all rights reserved.
#
#    This software is produced by the Supercomputing, Visualization &
#    e-Science Group, Manchester Computing, the Victoria University of
#    Manchester as part of the RealityGrid project.
#
#    This software has been tested with care but is not guaranteed for
#    any particular purpose. Neither the authors, nor the University of
#    Manchester offer any warranties or representations, nor do they
#    accept any liabilities with respect to this software.
#
#    This program must not be used for commmercial gain without the
#    written permission of the authors.
#    
#    Supercomputing, Visualization & e-Science Group
#    Manchester Computing
#    University of Manchester
#    Manchester M13 9PL
#
#    email:  csar-advice@cfs.ac.uk.
#    Tel:    +44 161 275 6824/5997
#    Fax:    +44 161 275 6040    
#    
#    Date          Version    Updates                            Author
#    ----          -------    -------                            ------
#    23.7.2002       0.1                                         A Porter
#
#---------------------------------------------------------------------------

# Set-up access to M4 scripts for production of F90 wrappers, this is
# taken from the PVM project.
ARCH           = SGI64
# include ${REG_STEER_HOME}/src/conf/${ARCH}.def
M4             = m4
M4FILE         = ${REG_STEER_HOME}/src/conf/${ARCH}.m4
RMCMD          = rm

# Whether to use 32 or 64-bit depends on contents of G and NBIT - these
# are passed to make on the cmd line
INCLUDE_PATH   = ${REG_STEER_HOME}/include
XML_PARSE_PATH = ${REG_STEER_HOME}/expat/xmlparse
LIB_PATH       = ${REG_STEER_HOME}/lib$(NBIT)
CC             = cc
CC_FLAGS       = -ansi $(G) -I${INCLUDE_PATH} -I${XML_PARSE_PATH}

.c.o:
	${CC} ${CC_FLAGS} -c $< -o $@

all:
	make ReG_Steer_Appside
	make ReG_Steer_Appside_f
	make ReG_Steer_Steerside
	make ReG_Steer_Common

appside:
	make ReG_Steer_Appside

appside_f:
	make ReG_Steer_Appside_f

steerside:
	make ReG_Steer_Steerside

common:
	make ReG_Steer_Common

ReG_Steer_Appside_f: ReG_Steer_Appside_f.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_f.o

ReG_Steer_Appside: ReG_Steer_Appside.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside.o

ReG_Steer_Steerside: ReG_Steer_Steerside.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside.o

ReG_Steer_Common: ReG_Steer_Common.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Common.o

ReG_Steer_Appside.o: ReG_Steer_Appside.c \
	 ${INCLUDE_PATH}/ReG_Steer_Appside.h \
	 ${INCLUDE_PATH}/ReG_Steer_types.h \
	 ${INCLUDE_PATH}/ReG_Steer_Appside_internal.h \
	 ${INCLUDE_PATH}/ReG_Steer_Common.h

ReG_Steer_Steerside.o: ReG_Steer_Steerside.c \
	 ${INCLUDE_PATH}/ReG_Steer_Steerside.h \
	 ${INCLUDE_PATH}/ReG_Steer_types.h \
	 ${INCLUDE_PATH}/ReG_Steer_Steerside_internal.h \
	 ${INCLUDE_PATH}/ReG_Steer_Common.h

ReG_Steer_Common.o: ReG_Steer_Common.c \
	 ${INCLUDE_PATH}/ReG_Steer_types.h \
	 ${INCLUDE_PATH}/ReG_Steer_Common.h

ReG_Steer_Appside_f.c: ReG_Steer_Appside_f.m4 \
	 ${M4FILE}
	${M4} ${M4FILE} ReG_Steer_Appside_f.m4 > $@

ReG_Steer_Appside_f.o: ReG_Steer_Appside_f.c \
         ${INCLUDE_PATH}/ReG_Steer_Appside_internal.h

ReG_Steer_Appside_f.c: ReG_Steer_Appside_f.m4

clean:
	rm -f ReG_Steer_Steerside.o ReG_Steer_Appside.o ReG_Steer.ln *~ 
	rm -f ReG_Steer_Common.o ReG_Steer_Appside_f.o
	rm -f ReG_Steer_Appside_f.c
	rm -f ${INCLUDE_PATH}/*~
	ar dv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside.o \
		ReG_Steer_Appside.o ReG_Steer_Appside_f.o ReG_Steer_Common.o

tidy:
	rm -f ReG_Steer_Steerside.o ReG_Steer_Appside.o \
	ReG_Steer_Common.o ReG_Steer_Appside_f.o ReG_Steer_Appside_f.c *~

