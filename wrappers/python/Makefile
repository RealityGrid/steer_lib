include ../../Makefile.include
include ../../make/Makefile.${ARCH}

lib:	ReG_Steer_wrap.o
	${CC} -shared -L${REG_STEER_HOME}/lib${NBIT} -lReG_Steer \
	ReG_Steer_wrap.o -o _ReG_Steer.so

ReG_Steer_wrap.o: ReG_Steer_wrap.c
	${CC} -c ReG_Steer_wrap.c -fpic -I/usr/include -I/usr/include/python2.2 \
	-I/usr/lib/python2.2/config/ -I${REG_STEER_HOME}/include -DHAVE_CONFIG_H

ReG_Steer_wrap.c:
	${REG_SWIG} -python -shadow ReG_Steer.i

clean:
	rm -rf *.pyc *.so ReG_Steer.py *_wrap.* *~ core
