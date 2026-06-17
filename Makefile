
.PHONY: default ALL LIB cleanall rundir

default: ALL

ifeq ($(wildcard build/Makefile.local),)
  $(error build/Makefile.local not found. Run ./config.sh --compiler=<gfortran|nagfor> to configure.)
endif
include build/Makefile.local

ALL:
	@cd src; make LIB
	@cd src/main; make DIRSFILE=${DIRSFILE} BUILDDIR=${BUILDDIR} TEST

cleanall:
	@echo "--> Cleaning srcIndices"
	@echo "  --> Current directory : `pwd`"
	@echo "  --> Removing library file in lib:"
	@rm -f lib/*.a
	@echo "  --> Removing files in src:"
	@cd src; make --no-print-directory clean
	@echo "--> Done Cleaning srcIndices"

rundir:
	rm -rf run
	mkdir run
	cd run ; ln -s ../src/io_test.exe ; ln -s ../data .

LIB:
	@cd src; make --no-print-directory SHARELIB

clean: cleanall
