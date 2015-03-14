# Author: Jorge. A Navas, The University of Melbourne 2012

include ./Makefile.conf

VERSION=0.1
EXEC_NAME=ftclp.${VERSION}
# Main .pl file
MAIN  = tclp 

all:
	@echo " ---------------------------------------------------------------"
	@echo "   Compiling and generating executable                          "
	@echo " ---------------------------------------------------------------"
	ln -sf ${MSAT_DIR}/lib/libmathsat.a $$FTCLP_INSTALL/lib
	cd solver && make 
	cd trie && make
	${CIAOC} -S  ${MAIN}
	mv ./${MAIN} ./bin/${EXEC_NAME}

clean:
	@echo " ----------------------------------------------------------------------"
	@echo "  Cleaning files"
	@echo " ----------------------------------------------------------------------"
	rm -f *.itf *.po *.pl~
	rm -f $$FTCLP_INSTALL/lib/libmathsat.a
	rm -Rf $$FTCLP_INSTALL/bin/${EXEC_NAME}
	cd solver && make clean
	cd trie && make clean
	cd adt && make clean
	cd analysis && make clean
	cd frontend && make clean
	cd tests && make clean

install:
	@echo " ---------------------------------------------------------------"
	@echo "   Installing third-party systems                               "
	@echo " ---------------------------------------------------------------"
	./install_ciao
	./install_gmp

uninstall:
	@echo " ---------------------------------------------------------------"
	@echo "   Uninstalling                                                 "
	@echo " ---------------------------------------------------------------"
	rm -Rf third-party/ciao
	rm -Rf third-party/gmp-6.0.0
	cd solver && make uninstall
	cd trie && make uninstall
	rm -Rf $$FTCLP_INSTALL/lib/*.a
	rm -Rf $$FTCLP_INSTALL/bin/${EXEC_NAME}



