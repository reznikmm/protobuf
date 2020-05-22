PROTOBUF_DIR ?= $(GITHUB_WORKSPACE)/google
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
	gprbuild $(GPRBUILD_FLAGS) -P gnat/protoc_gen_ada.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/protobuf_runtime.gpr
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/protoc_gen_ada.gpr --mode=usage

clean:
	gprclean -q -P gnat/protoc_gen_ada.gpr
	gprclean -q -P gnat/protobuf_runtime.gpr

check:
	@echo Compile some predefined .proto files
	for J in any duration empty field_mask struct timestamp wrappers; do\
	  echo $$J; PATH=.objs/compiler/:$$PATH \
	  protoc --ada_out=source/runtime/generated \
	   /usr/include/google/protobuf/$$J.proto; done
	gprbuild -P gnat/protobuf_runtime.gpr
	@echo Run conformance test if any
	./conformance.sh $(PROTOBUF_DIR)
