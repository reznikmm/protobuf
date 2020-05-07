GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
BINDIR                 ?= $(PREFIX)/bin
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/ada-protobuf
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_BIN_DIR        ?= $(DESTDIR)$(BINDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/ada-protobuf

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --link-lib-subdir=$(INSTALL_LIBRARY_DIR) --exec-subdir=$(INSTALL_BIN_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/protobuf_runtime.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/compiler.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/protobuf_runtime.gpr
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/compiler.gpr --mode=usage

clean:
	gprclean -q -P gnat/compiler.gpr
	gprclean -q -P gnat/protobuf_runtime.gpr

