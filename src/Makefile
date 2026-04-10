
default : LIB

# Check which makefile.def to use by default:
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

# ModExtras provides ModKind, ModIoUnit, ModErrors, ModTimeConvert for standalone
# This can come from elsewhere or be used from ModExtras.
# Use PreProc (in Makefile.def) to specify what's needed 
# (#STANDALONE for all, #NEEDMODTIMECONVERT, etc.)


OBJECTS = \
	ModIndConsts.o\
	ModExtras.o\
	ModF107.o\
	ModAE.o\
	ModHPI.o\
	ModIMF.o\
	ModIndices.o

DEPEND:
	@perl ${SCRIPTDIR}/depend.pl  ${SEARCH} ${LIBDIR} ${OBJECTS}

LIB:
	@make ${MYLIB}
	@echo

${MYLIB}: ${OBJECTS}
	@echo
	@echo 'creating LibIE'
	@echo
	@rm -f ${MYLIB}
	${AR} ${MYLIB} ${OBJECTS}

SHARELIB: DEPEND
	@make ${MYLIB}
