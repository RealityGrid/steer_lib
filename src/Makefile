#    Makefile for RealityGrid steering library.
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
#    Initial version by:  A Porter, 23.7.2002
#
#------------------------------------------------------------------------

# Set-up access to M4 scripts for production of F90 wrappers, this is
# taken from the PVM project.
ARCH           = LINUX
BIN_DIR        = ${HOME}/bin
JAVAC          = javac
M4             = m4

M4FILE         = ${REG_STEER_HOME}/src/conf/${ARCH}.m4

# Whether to use 32 or 64-bit depends on contents of G and NBIT - these
# are passed to make on the cmd line
XML_INCLUDE_PATH = ${REG_XML_INCDIR}

REG_INCLUDE_PATH = ${REG_STEER_HOME}/include
INCLUDE_PATH   = -I${REG_INCLUDE_PATH}  \
                 -I${XML_INCLUDE_PATH}

LIB_PATH       = ${REG_STEER_HOME}/lib$(NBIT)
CC             = cc
CC_FLAGS       = -ansi $(G) ${INCLUDE_PATH}

.c.o:
	${CC} ${CC_FLAGS} -c $< -o $@

${LIB_PATH}/libReG_Steer.a: ReG_Steer_Appside_f.o \
	ReG_Steer_Appside.o \
	ReG_Steer_Steerside.o \
	ReG_Steer_Common.o \
	ReG_Steer_Proxy_utils.o \
	${BIN_DIR}/ReG_Steer_Proxy.class \
	ReG_Steer_XML.o

${BIN_DIR}/ReG_Steer_Proxy.class: *.java

${LIB_PATH}/libReG_Steer.a:
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside_f.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Appside.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Common.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Proxy_utils.o
	ar rv ${LIB_PATH}/libReG_Steer.a ReG_Steer_XML.o

${BIN_DIR}/ReG_Steer_Proxy.class:
	${JAVAC} ReG_Steer_Proxy.java
	mv -f ReG_Steer_Proxy.class ${BIN_DIR}/.

#---------------------------------------------------------------

ReG_Steer_Appside.o: ReG_Steer_Appside.c \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Appside.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_types.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Appside_internal.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Common.h

ReG_Steer_Steerside.o: ReG_Steer_Steerside.c \
         ReG_Steer_Proxy.java \
         ReG_Steer_Proxy_utils.c \
	 ReG_Steer_XML.c \
	 ${REG_INCLUDE_PATH}/ReG_Steer_XML.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Proxy_utils.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Steerside.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_types.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Steerside_internal.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Common.h

ReG_Steer_Common.o: ReG_Steer_Common.c \
	 ${REG_INCLUDE_PATH}/ReG_Steer_types.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Common.h

ReG_Steer_Proxy_utils.o: ReG_Steer_Proxy_utils.c \
	 ${REG_INCLUDE_PATH}/ReG_Steer_types.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Proxy_utils.h

ReG_Steer_Appside_f.c: ReG_Steer_Appside_f.m4 \
	 ${M4FILE}
	${M4} ${M4FILE} ReG_Steer_Appside_f.m4 > $@

ReG_Steer_Appside_f.o: ReG_Steer_Appside_f.c \
         ${REG_INCLUDE_PATH}/ReG_Steer_Appside_internal.h \
         ${REG_INCLUDE_PATH}/ReG_Steer_Appside.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_types.h \
	 ${REG_INCLUDE_PATH}/ReG_Steer_Common.h

ReG_Steer_XML.o: ReG_Steer_XML.c \
	${REG_INCLUDE_PATH}/ReG_Steer_XML.h \
	${REG_INCLUDE_PATH}/ReG_Steer_types.h

#---------------------------------------------------------------

clean:
	rm -f *.o ReG_Steer.ln *~ 
	rm -f ReG_Steer_Appside_f.c
	rm -f ${REG_INCLUDE_PATH}/*~
	rm -f ${BIN_DIR}/ReG_Steer_Proxy.class
	ar dv ${LIB_PATH}/libReG_Steer.a ReG_Steer_Steerside.o \
		ReG_Steer_Appside.o ReG_Steer_Appside_f.o \
                ReG_Steer_Common.o ReG_Steer_Proxy_utils.o

tidy:
	rm -f ReG_Steer_Steerside.o ReG_Steer_Appside.o \
	ReG_Steer_Common.o ReG_Steer_Appside_f.o ReG_Steer_Appside_f.c *~

