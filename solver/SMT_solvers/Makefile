include ../../Makefile.conf

## for debugging purposes
RUN_TEST=0

CXX = g++
CXXFLAGS = -g -O3 # -fPIC 
ifeq ($(COMPILE_ON_32_BITS),1)
CXXFLAGS  += -m32
endif 
CXXFLAGS += -I. -I$$FTCLP_INSTALL -I${CIAO_INCLUDE}
CXXFLAGS += -Wall -Wno-non-virtual-dtor -Wno-unused-function -Wno-deprecated -ffast-math 
# Mathsat
CXXFLAGS += -I$(MSAT_DIR)/include -I$(GMP_INCLUDE)
ifeq ($(RUN_TEST),1)
LFLAGS    = -L$(MSAT_DIR)/lib -L$(GMP_LIB) -lz -lmathsat -lgmpxx -lgmp -lstdc++
endif 
# Z3
# CXXFLAGS += -I${Z3_DIR}/include -I${IZ3_DIR}/include
# LFLAGS   += -L${Z3_DIR}/lib -L${IZ3_DIR}/lib -L${FOCI_LIB}
# LFLAGS   += -lz3 -liz3 -lfoci 

SRCS = MSat.cpp  # Z3.cpp
OBJS = $(addsuffix .o, $(basename $(SRCS)))
DEPS = $(addsuffix .d, $(basename $(SRCS)))

ifeq ($(RUN_TEST),1)
TESTSRCS = test_MSat.cpp
TESTS = $(basename $(TESTSRCS))
TARGETS = $(TESTS)
else
TARGETS = $(basename $(SRCS))
endif 

TARGETOBJS = $(addsuffix .o, $(TARGETS))
TARGETDEPS = $(addsuffix .d, $(TARGETS))
all : $(TARGETS)

ifeq ($(RUN_TEST),1)
$(TARGETS) : % : %.o $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $@.o $(OBJS) $(LFLAGS)
else
$(TARGETS) : 
	$(CXX) $(CXXFLAGS) -c $< $(SRCS)
endif 

clean :
	rm -f $(TARGETS) $(TARGETOBJS) $(OBJS) $(TARGETDEPS) $(DEPS)

-include $(DEPS)
-include $(TARGETDEPS)

%.d: %.cpp
	@$(CXX) -MM -MT  $(subst .d,.o,$@) -MT $@ $(CPPFLAGS) $< > $@
