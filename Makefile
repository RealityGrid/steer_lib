# Edit the two following lines if 32-bit binaries are desired 
# (NBIT = 32 and G=-n32).
NBIT         = 32
GLOBAL_FLAGS = G=-n32 NBIT=$(NBIT)
LOCAL_BIN_DIR= ${HOME}/bin

all:
	make app
	make app_f90
	make app_f90_parallel
	make steerer

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

lib$(NBIT)/libReG_Steer.a: expat/xmlparse/libexpat.a \
 include/*.h \
 src/*.c src/*.m4 src/*.java
	cd src; make ${GLOBAL_FLAGS}

${LOCAL_BIN_DIR}/ReG_Steer_Proxy.class: src/*.java
	cd src; make  ${GLOBAL_FLAGS}

expat/xmlparse/libexpat.a: 
	cd expat; make ${GLOBAL_FLAGS}

clean:
	cd expat; make ${GLOBAL_FLAGS} clean
	cd src; make ${GLOBAL_FLAGS} clean 
	cd examples/mini_app; make ${GLOBAL_FLAGS} clean
	cd examples/mini_app_f90; make ${GLOBAL_FLAGS} clean
	cd examples/mini_app_f90_parallel; make ${GLOBAL_FLAGS} clean
	cd examples/mini_steerer; make ${GLOBAL_FLAGS} clean

tar:
	make clean
	cd ..; tar -cf reg_steer_backup.tar reg_steer_lib/*; \
gzip reg_steer_backup.tar
