GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/protobuf
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/protobuf

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
--link-lib-subdir=$(INSTALL_LIBRARY_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/protobuf_support.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/compiler.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/protobuf_support.gpr
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/compiler.gpr

clean:
	gprclean -q -P gnat/compiler.gpr
	gprclean -q -P gnat/protobuf_support.gpr

