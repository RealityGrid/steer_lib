# Edit the two following lines if 32-bit binaries are desired 
# (NBIT = 32 and G=-n32).
NBIT         = 64
GLOBAL_FLAGS = G=-64 NBIT=$(NBIT)

all:
	make app
	make app_f90
	make steerer

app: lib$(NBIT)/libReG_Steer.a \
 mini_app/*.c
	cd mini_app; make ${GLOBAL_FLAGS}

app_f90: lib$(NBIT)/libReG_Steer.a \
 include/*.inc \
 mini_app_f90/*.f90
	cd mini_app_f90; make ${GLOBAL_FLAGS}

steerer: lib$(NBIT)/libReG_Steer.a \
 mini_steerer/*.c
	cd mini_steerer; make ${GLOBAL_FLAGS}

lib$(NBIT)/libReG_Steer.a: expat/xmlparse/libexpat.a \
 include/*.h \
 src/*.c src/*.m4
	cd src; make ${GLOBAL_FLAGS}

expat/xmlparse/libexpat.a: 
	cd expat; make ${GLOBAL_FLAGS}

clean:
	cd expat; make ${GLOBAL_FLAGS} clean
	cd src; make ${GLOBAL_FLAGS} clean 
	cd mini_app; make ${GLOBAL_FLAGS} clean
	cd mini_app_f90; make ${GLOBAL_FLAGS} clean
	cd mini_steerer; make ${GLOBAL_FLAGS} clean

tar:
	make clean
	cd ..; tar -cf reg_steer_backup.tar reg_steer/*; \
gzip reg_steer_backup.tar
