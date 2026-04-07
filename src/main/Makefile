
default : TEST

include ../../build/Makefile.def

SEARCH = -I..

EXE = ../io_test.exe

OBJECTS = \
	ModMain.o\
	main.o

${EXE}: ${OBJECTS}
	${LINK.f90} -o ${EXE} ${OBJECTS} -L../../lib -lIO

TEST:
	@make ${EXE}

