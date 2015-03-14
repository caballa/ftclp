include ../../Makefile.conf

CC= gcc
CFLAGS= -Wall -g # -pedantic -std=c89
CFLAGS += -I$$FTCLP_INSTALL

ifeq ($(COMPILE_ON_32_BITS),1)
CFLAGS += -m32
LDFLAGS += -m32
endif 

ifeq ($(SHARED_LIBS),1)
CFLAGS += -fPIC
LDFLAGS += -shared 
endif 

#CFLAGS  += -pg
#CFLAGS  += -fprofile-arcs -ftest-coverage
#LDFLAGS += -fprofile-arcs -ftest-coverage

LIBNAME= libcritbit
OBJS= critbit.o

all: $(OBJS)

# ifeq ($(SHARED_LIBS),1)
# $(LIBNAME).so: $(OBJS)
# 	$(CC) $(LFLAGS) $(LDFLAGS) -Wl,-soname,$(LIBNAME).so -o $(LIBNAME).so $(OBJS)
# 	mv ./$(LIBNAME).so $(FTCLP_INSTALL)/lib
# else
# $(LIBNAME).a: $(OBJS)
# 	ar rcs $(LIBNAME).a $(OBJS)
# 	mv ./$(LIBNAME).a $(FTCLP_INSTALL)/lib
# endif 

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

tests: critbit.o test-1.o test-2.o
	$(CC) $(LDFLAGS) critbit.o test-1.o -o test-1
	$(CC) $(LDFLAGS) critbit.o test-2.o -o test-2

clean:
	rm -rf *.o 
	rm -rf *.o test-1 test-2

uninstall:

# uninstall:
# 	rm -rf $(FTCLP_INSTALL)/lib/$(LIBNAME).a
# 	rm -rf $(FTCLP_INSTALL)/lib/$(LIBNAME).so


