CC       = cc
CC_FLAGS = -O2 -c -I../xmlparse
TARGET   = reg_expt

elements:  ${TARGET}.o
	${CC} ${TARGET}.o -o elements -L../xmlparse -lexpat

.c.o:
	${CC} ${CC_FLAGS} -c $< -o $@

