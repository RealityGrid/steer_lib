include ../Makefile.include

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
	cd python; ${MAKE} clean
	cd perl;   ${MAKE} clean
	cd tcl;    ${MAKE} clean
	cd java;   ${MAKE} clean

FORCE:
