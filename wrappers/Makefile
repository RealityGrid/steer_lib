include ../Makefile.include
include Makefile.wrap

all:
	${MAKE} ${REG_WRAP}

python: FORCE
	cd python; ${MAKE}

perl: FORCE
	cd perl; ${MAKE}

java: FORCE
	cd java; ${MAKE}

tcl: FORCE
	cd tcl; ${MAKE}

none:

clean:
	rm -rf *~ core
	cd python; ${MAKE} clean
	cd perl;   ${MAKE} clean
	cd tcl;    ${MAKE} clean
	cd java;   ${MAKE} clean

FORCE:
