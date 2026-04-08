
default : LIB

include ../build/Makefile.def

# include ../../../Makefile.def
# include ../../../Makefile.conf

# include Makefile.DEPEND

MYLIB = ../lib/libIO.a

OBJECTS = \
	ModKind.o\
	ModCharSize.o\
	ModErrors.o\
	ModTime.o\
	ModIoUnit.o\
	ModF107.o\
	ModSME.o\
	ModIndices.o

# DEPEND:
# 	@perl ${SCRIPTDIR}/depend.pl  ${SEARCH} ${LIBDIR} ${OBJECTS}

LIB:
	@make ${MYLIB}
	@echo

${MYLIB}: ${OBJECTS}
	@echo
	@echo 'creating LibIE'
	@echo
	@rm -f ${MYLIB}
	${AR} ${MYLIB} ${OBJECTS}

# SHARELIB: DEPEND
# 	@make ${MYLIB}
