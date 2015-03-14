include ../Makefile.conf

CXX = g++
CXXFLAGS  = -g  -O3 
ifeq ($(COMPILE_ON_32_BITS),1)
CXXFLAGS += -m32
LDFLAGS += -m32
endif 
ifeq ($(SHARED_LIBS),1)
CFLAGS += -fPIC
LDFLAGS += -shared 
endif 

CXXFLAGS +=  -I${CIAO_INCLUDE} -I. -I$$FTCLP_INSTALL
CXXFLAGS += -Wall -c -Wno-non-virtual-dtor -Wno-unused-function -Wno-deprecated -ffast-math 
# MSat
CXXFLAGS += -I$(GMP_INCLUDE) -I$(MSAT_DIR)/include 
LFLAGS   += -L$(MSAT_DIR)/lib -L$(GMP_LIB) -lz -lmathsat -lgmpxx -lgmp -lstdc++
# Z3
# CXXFLAGS += -I${Z3_DIR}/include -I${IZ3_DIR}/include
# LFLAGS   += -L${Z3_DIR}/lib -L${IZ3_DIR}/lib -L${FOCI_LIB}
# LFLAGS   += -lz3 -liz3 -lfoci 

SRCS    = CiaoSMT.cpp ./SMT_solvers/MSat.cpp # ./SMT_solvers/Z3.cpp
OBJS    = $(SRCS:.cpp=.o)
LIB	= libciao_smt

all: 
	cd SMT_solvers && make all	
	$(CXX) $(CXXFLAGS) $(LFLAGS) CiaoSMT.cpp
ifeq ($(SHARED_LIBS),1)
	$(CXX) $(LFLAGS) $(LDFLAGS) -Wl,-soname,$(LIB).so -o $(LIB).so $(OBJS)
	mv ./$(LIB).so $(FTCLP_INSTALL)/lib	
else
	ar rcs $(LIB).a $(OBJS)
	mv ./$(LIB).a $(FTCLP_INSTALL)/lib	
endif 

.cpp.o:
	$(CXX) $(CXXFLAGS) $(LFLAGS) $< $@

clean :
	cd SMT_solvers && make clean
	rm -f $(OBJS) 
	rm -f *_glue.*
	rm -f *.itf
	rm -f *.po
	rm -rf $(FTCLP_INSTALL)/lib/$(LIB).a
	rm -rf $(FTCLP_INSTALL)/lib/$(LIB).so

uninstall:




