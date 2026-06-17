
default : LIB

ifeq ($(wildcard ../build/Makefile.local),)
  $(error ../build/Makefile.local not found. Run config.sh from the srcIndices root to configure.)
endif
include ../build/Makefile.local

include $(DIRSFILE)
ifneq ($(wildcard $(BUILDDIR)/Makefile.conf),)
  include $(BUILDDIR)/Makefile.conf
endif

include Makefile.DEPEND

MYLIB = ../lib/libIO.a

OBJECTS = \
	ModIndConsts.o\
	ModExtras.o\
	ModF107.o\
	ModAE.o\
	ModHPI.o\
	ModIMF.o\
	ModIndices.o

# Fallback if ${SCRIPTDIR}/depend.pl cannot be found.
# It is gitm-specific. this allows Makefile.DEPEND to be created in tiegcm

DEPEND:
	@if [ -f "${SCRIPTDIR}/depend.pl" ]; then \
		perl ${SCRIPTDIR}/depend.pl ${SEARCH} ${LIBDIR} ${OBJECTS}; \
	else \
		perl ${SCRIPTDIR}/mkdepends . ext && mv Depends Makefile.DEPEND; \
	fi

Makefile.DEPEND: DEPEND

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

