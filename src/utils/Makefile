F90_FLAGS = 
CC_FLAGS  = 

# Set-up access to M4 scripts for production of F90 wrappers, this is
# taken from the PVM project.
ARCH           = SGI
M4             = m4
M4FILE         = ${REG_STEER_HOME}/src/conf/${ARCH}.m4

all:
	make sizeof_f
	make max_kind
	make c_types

sizeof_f: sizeof_f.m4
	${M4} ${M4FILE} sizeof_f.m4 > sizeof_f.c
	cc ${CC_FLAGS} -c sizeof_f.c -o sizeof_f.o

max_kind: max_kind.f90 sizeof_f.m4
	f90 ${F90_FLAGS} -c max_kind.f90 -o max_kind.o
	f90 ${F90_FLAGS} max_kind.o sizeof_f.o -o max_kind

c_types: c_types.c
	cc ${CC_FLAGS} c_types.c -o c_types

clean:
	rm -f *~ *.o max_kind c_types sizeof_f.c
 
