SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
DEP = $(OBJ:.o=.d)

CFLAGS:=-Wall -O2 -pthread -fPIC
CC:=gcc

ASFLAGS :=

SONAME:=libfast-lzma2.so.1
REAL_NAME:=libfast-lzma2.so.1.0
LINKER_NAME=libfast-lzma2.so

UNAME_P:=$(shell uname -p)
ifeq ($(UNAME_P),x86_64)
	ASFLAGS+=-D MS_x64_CALL=0
	CFLAGS+=-DLZMA2_DEC_OPT
	OBJ+=../lzma_dec_x86_64.o
endif

libfast-lzma2 : $(OBJ)
	$(CC) -shared -pthread -Wl,-soname,$(SONAME) -o $(REAL_NAME) $(OBJ)

-include $(DEP)

%.d: %.c
	@$(CC) $(CFLAGS) $< -MM -MT $(@:.d=.o) >$@

DESTDIR:=
PREFIX:=/usr/local
LIBDIR:=$(DESTDIR)$(PREFIX)/lib

.PHONY: install
install:
	mkdir -p $(LIBDIR)
	cp $(REAL_NAME) $(LIBDIR)/$(REAL_NAME)
	strip $(LIBDIR)/$(REAL_NAME)
	chmod 0755 $(LIBDIR)/$(REAL_NAME)
	ln -s $(LIBDIR)/$(REAL_NAME) $(LIBDIR)/$(LINKER_NAME)
	ldconfig -n $(LIBDIR)
	mkdir -p $(DESTDIR)$(PREFIX)/include
	cp fast-lzma2.h $(DESTDIR)$(PREFIX)/include/
	cp fl2_errors.h $(DESTDIR)$(PREFIX)/include/

.PHONY: uninstall
uninstall:
	rm -f $(LIBDIR)/$(REAL_NAME)
	ldconfig -n $(LIBDIR)
	rm -f $(DESTDIR)$(PREFIX)/include/fast-lzma2.h
	rm -f $(DESTDIR)$(PREFIX)/include/fl2_errors.h

.PHONY: clean
clean:
	rm -f $(REAL_NAME) $(OBJ) $(DEP)