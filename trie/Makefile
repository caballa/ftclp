include ../Makefile.conf

CRITBIT_INCLUDE=./critbit

CXX = g++
CXXFLAGS  = -g -O3 

ifeq ($(COMPILE_ON_32_BITS),1)
CXXFLAGS  += -m32
LDFLAGS += -m32
endif 

CXXFLAGS += -I. -I$$FTCLP_INSTALL
CXXFLAGS += -Wall -c -Wno-non-virtual-dtor -Wno-unused-function -Wno-deprecated -ffast-math 
CXXFLAGS += -I${CRITBIT_INCLUDE} -I${CIAO_INCLUDE}
LFLAGS   += -L$(FTCLP_INSTALL)/lib -lcritbit

ifeq ($(SHARED_LIBS),1)
LDFLAGS  += -shared 
endif 

SRCS    = CiaoTrie.cpp 
OBJS    = $(SRCS:.cpp=.o)
LIB     = libciao_trie

CRITBIT_OBJS= critbit/critbit.o

all: 
	cd critbit && make 
	$(CXX) $(CXXFLAGS) $(LFLAGS) CiaoTrie.cpp
ifeq ($(SHARED_LIBS),1)
	$(CXX) $(LFLAGS) $(LDFLAGS) -Wl,-soname,$(LIB).so -o $(LIB).so $(OBJS) $(CRITBIT_OBJS)
	mv ./$(LIB).so $(FTCLP_INSTALL)/lib
else
	ar rcs $(LIB).a $(OBJS) $(CRITBIT_OBJS)
	mv ./$(LIB).a $(FTCLP_INSTALL)/lib
endif 

.cpp.o:
	$(CXX) $(CXXFLAGS) $(LFLAGS) $< $@

clean :
	cd critbit && make clean
	rm -f $(OBJS) 
	rm -f *_glue.*
	rm -f *.itf
	rm -f *.po
	rm -f $(FTCLP_INSTALL)/lib/$(LIB).so
	rm -f $(FTCLP_INSTALL)/lib/$(LIB).a

uninstall:
	cd critbit && make uninstall


