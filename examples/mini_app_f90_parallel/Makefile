# Makefile for mini-application
#
# G and NBIT are passed to make on cmd line

TARGET   = mini_app_para

F90       = f90
F90_FLAGS = -C $(G)

STEER_LIBS = -L${REG_STEER_HOME}/lib$(NBIT)
XML_LIBS   = -L${REG_STEER_HOME}/expat/xmlparse

.f90.o:
	${F90} ${F90_FLAGS} -I${REG_STEER_HOME}/include -c $< -o $@

${TARGET}: ${TARGET}.o \
mini_app_para.f90 \
${REG_STEER_HOME}/include/reg_steer_f90.inc
	${F90} ${F90_FLAGS} ${TARGET}.o -o $@ ${XML_LIBS} ${STEER_LIBS} -lmpi -lReG_Steer -lexpat
	mv ${TARGET} ${HOME}/bin/.

clean:
	rm -f *.o *~ core ${TARGET}

