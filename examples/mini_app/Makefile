# Makefile for mini-application
#
# G and NBIT are passed to make on cmd line

TARGET   = mini_app
CC       = cc
CC_FLAGS = -ansi $(G) -I${REG_STEER_HOME}/include

STEER_LIBS = -L${REG_STEER_HOME}/lib$(NBIT)
XML_LIBS   = -L${REG_STEER_HOME}/expat/xmlparse

.c.o:
	${CC} ${CC_FLAGS} -c $< -o $@

${TARGET}: ${TARGET}.o
	${CC} ${CC_FLAGS} ${TARGET}.o -o $@ \
${XML_LIBS} ${STEER_LIBS} -lReG_Steer -lexpat
	mv ${TARGET} ${HOME}/bin/.

clean:
	rm -f *.o *~ core ${TARGET}

