include ../../Makefile.include
include ../../make/Makefile.${ARCH}

REG_PACKAGE=org.realitygrid.steering
REG_PACKAGE_DIR=org/realitygrid/steering

examples: libReG_SteerJava.so
	${JAVAC} ${REG_PACKAGE_DIR}/*.java
	${JAVAC} examples/*.java

libReG_SteerJava.so: ReG_Steer_wrap.o
	${CC} -shared -L${REG_STEER_HOME}/lib${NBIT} -lReG_Steer \
	ReG_Steer_wrap.o -o libReG_SteerJava.so

ReG_Steer_wrap.o: ReG_Steer_wrap.c
	${CC} -c ReG_Steer_wrap.c -fpic -I/usr/include \
	-I/usr/java/j2sdk1.4.2_01/include -I/usr/java/j2sdk1.4.2_01/include/linux \
	-I${REG_STEER_HOME}/include

ReG_Steer_wrap.c:
	${REG_SWIG} -java -package ${REG_PACKAGE} \
	-outdir ${REG_PACKAGE_DIR} ReG_Steer.i

clean:
	rm -rf libReG_SteerJava.so *_wrap.* *.class *~ core
	rm -rf ${REG_PACKAGE_DIR}/SWIGTYPE*.java
	rm -rf ${REG_PACKAGE_DIR}/*p.java
	rm -rf ${REG_PACKAGE_DIR}/ReG_Steer*.java
	rm -rf ${REG_PACKAGE_DIR}/*.class
	rm -rf examples/*.class
	rm -rf examples/*~
