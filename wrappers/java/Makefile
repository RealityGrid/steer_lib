#  This file is part of the RealityGrid Steering Library Java Wrappers.
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
#  Author........: Robert Haines

include ../../Makefile.include
include ../../make/Makefile.${ARCH}
include ../Makefile.wrap

REG_PACKAGE=org.realitygrid.steering
REG_PACKAGE_DIR=org/realitygrid/steering

examples: libReG_SteerJava.so
	${JAVAC} -classpath ${REG_STEER_HOME}/wrappers/java ${REG_PACKAGE_DIR}/*.java
	${JAVAC} -classpath ${REG_STEER_HOME}/wrappers/java examples/*.java

libReG_SteerJava.so: ReG_Steer_wrap.o
	${CC} -shared ${REG_WRAP_LINK} \
	ReG_Steer_wrap.o -o libReG_SteerJava.so

ReG_Steer_wrap.o: ReG_Steer_wrap.c
	${CC} -c ReG_Steer_wrap.c -fpic -I/usr/include \
	${REG_JAVA_INC} -I${REG_STEER_HOME}/include

ReG_Steer_wrap.c: ReG_Steer.i
	${REG_SWIG} -java -package ${REG_PACKAGE} \
	-outdir ${REG_PACKAGE_DIR} ReG_Steer.i

clean:
	rm -rf libReG_SteerJava.so *_wrap.* *.class *~ core
	rm -rf ${REG_PACKAGE_DIR}/SWIGTYPE*.java
	rm -rf ${REG_PACKAGE_DIR}/*p.java
	rm -rf ${REG_PACKAGE_DIR}/ReG_Steer.java
	rm -rf ${REG_PACKAGE_DIR}/ReG_SteerJNI.java
	rm -rf ${REG_PACKAGE_DIR}/ReG_SteerConstants.java
	rm -rf ${REG_PACKAGE_DIR}/*.class
	rm -rf ${REG_PACKAGE_DIR}/*~
	rm -rf examples/*.class
	rm -rf examples/*~
