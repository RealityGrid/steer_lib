include ../../Makefile.include
include ../../make/Makefile.${ARCH}
include ../Makefile.wrap

lib:	ReG_Steer_wrap.o
	${CC} -shared -L${REG_STEER_HOME}/lib${NBIT} -lReG_Steer \
	ReG_Steer_wrap.o -o _ReG_Steer.so

ReG_Steer_wrap.o: ReG_Steer_wrap.c
	${CC} -c ReG_Steer_wrap.c -fpic -I/usr/include \
	-I${REG_STEER_HOME}/include ${REG_PYTHON_INC} 

ReG_Steer_wrap.c: ReG_Steer.i
	${REG_SWIG} -python -shadow ReG_Steer.i

clean:
	rm -rf *.pyc *.so ReG_Steer.py *_wrap.* *~ core
	rm -rf examples/*~
