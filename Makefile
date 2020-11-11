# To build in production mode, do  "make LIBRARY_TYPE=static BUILD_MODE=prod".
# To build in development mode, do "make LIBRARY_TYPE=static BUILD_MODE=dev".

BUILD_MODE ?= dev
LIBRARY_TYPE ?= static
LALTOOLS_SET ?= all
PROCESSORS ?= 0

LIB_PROJECTS = \
   src/build.gpr

TESTSUITE_PROJECTS = \
   testsuite/ada_drivers/refactor_imports/refactor_imports.gpr \
   testsuite/ada_drivers/outgoing_calls/outgoing_calls.gpr \
   testsuite/ada_drivers/refactoring_rename/rename.gpr \

ALL_PROJECTS = \
   $(LIB_PROJECTS) $(TESTSUITE_PROJECTS)

.PHONY: lib
lib:
	which gprbuild
	which gcc
	for proj in $(LIB_PROJECTS) ; do \
          gprbuild -v -k \
	   -XLIBRARY_TYPE=$(LIBRARY_TYPE) \
	   -XXMLADA_BUILD=$(LIBRARY_TYPE) \
	   -XBUILD_MODE=$(BUILD_MODE) \
	   -XLALTOOLS_SET=$(LALTOOLS_SET) \
	   -P $$proj -p -j$(PROCESSORS) ; \
        done

.PHONY: testsuite_drivers
testsuite_drivers:
	which gprbuild
	which gcc
	for proj in $(TESTSUITE_PROJECTS) ; do \
          gprbuild -v -k \
	   -XLIBRARY_TYPE=$(LIBRARY_TYPE) \
	   -XXMLADA_BUILD=$(LIBRARY_TYPE) \
	   -XBUILD_MODE=$(BUILD_MODE) \
	   -XLALTOOLS_SET=$(LALTOOLS_SET) \
	   -P $$proj -p -j$(PROCESSORS) ; \
        done

.PHONY: all
all:
	which gprbuild
	which gcc
	for proj in $(ALL_PROJECTS) ; do \
          gprbuild -v -k \
	   -XLIBRARY_TYPE=$(LIBRARY_TYPE) \
	   -XXMLADA_BUILD=$(LIBRARY_TYPE) \
	   -XBUILD_MODE=$(BUILD_MODE) \
	   -XLALTOOLS_SET=$(LALTOOLS_SET) \
	   -P $$proj -p -j$(PROCESSORS) ; \
        done


.PHONY: test
test: all
	bin/utils-var_length_ints-test
	testsuite/testsuite.py

install-strip:
	mkdir -p "$(DESTDIR)"
	cp -r bin "$(DESTDIR)/"
	# Don't strip debug builds
	test "$(BUILD_MODE)" = dev || strip "$(DESTDIR)/bin/"*


.PHONY: clean
clean:
	rm -rf obj bin
	rm -rf testsuite/ada_drivers/bin
	rm -rf testsuite/ada_drivers/**/obj
