
default : LIB

# Check which makefile.def to use:
# $(wildcard path/to/file) returns a path if the file exists (empty str if not)
# So if file exists, parens will =/= and if-block is entered
# > If file DNE, () are equal and we include ../build/Makefile.def
ifneq ($(wildcard ../../../Makefile.def),)
  include ../../../Makefile.def
else
  include ../build/Makefile.def
endif


-include ../../../Makefile.conf
-include Makefile.DEPEND

MYLIB = ../lib/libIO.a

# ModExtras.o provides ModKind, ModIoUnit, ModErrors, ModTimeConvert for standalone
# Inside other models, this comes from elsewhere
MODULES = \
	ModExtras.o

OBJECTS = \
	ModF107.o\
	ModAE.o\
	ModIMF.o\
	ModIndices.o

${OBJECTS}: ${MODULES}

DEPEND:
	@perl ${SCRIPTDIR}/depend.pl  ${SEARCH} ${LIBDIR} ${OBJECTS}

LIB:
	@make ${MYLIB}
	@echo

${MYLIB}: ${MODULES} ${OBJECTS}
	@echo
	@echo 'creating LibIE'
	@echo
	@rm -f ${MYLIB}
	${AR} ${MYLIB} ${MODULES} ${OBJECTS}

SHARELIB: DEPEND
	@make ${MYLIB}
