# To build in production mode, do  "make LIBRARY_TYPE=static BUILD_MODE=prod".
# To build in development mode, do "make LIBRARY_TYPE=static BUILD_MODE=dev".

BUILD_MODE ?= dev
LIBRARY_TYPE ?= static
LALTOOLS_SET ?= all
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable
ALL_BUILD_MODES = dev prod AddressSanitizer

LIB_PROJECTS = \
   src/lal_tools.gpr

BIN_PROJECTS = \
   src/build.gpr

TESTSUITE_PROJECTS = \
   testsuite/ada_drivers/partial_gnatpp/partial_gnatpp.gpr \
   testsuite/ada_drivers/refactor_imports/refactor_imports.gpr \
   testsuite/ada_drivers/outgoing_calls/outgoing_calls.gpr \
   testsuite/ada_drivers/refactoring_safe_rename/safe_rename.gpr \
   testsuite/ada_drivers/refactoring_add_parameter/add_parameter.gpr \
   testsuite/ada_drivers/refactoring_remove_parameter/remove_parameter.gpr \
   testsuite/ada_drivers/move_parameter/move_parameter.gpr \
   testsuite/ada_drivers/change_parameter_mode/change_parameter_mode.gpr \
   testsuite/ada_drivers/refactoring_suppress_separate/suppress_separate.gpr \
   testsuite/ada_drivers/refactoring_extract_subprogram/extract_subprogram.gpr \
   testsuite/ada_drivers/refactoring_pull_up_declaration/pull_up_declaration.gpr \
   testsuite/ada_drivers/refactoring_change_parameters_type/change_parameters_type.gpr \
   testsuite/ada_drivers/refactoring_change_parameters_default_value/change_parameters_default_value.gpr \
   testsuite/ada_drivers/refactoring_introduce_parameter/introduce_parameter.gpr

ALL_PROJECTS = \
   $(BIN_PROJECTS) $(LIB_PROJECTS) $(TESTSUITE_PROJECTS)

.PHONY: lib
lib:
	which gprbuild
	which gcc
	for proj in $(LIB_PROJECTS) ; do \
	  for kind in $(ALL_LIBRARY_TYPES) ; do \
	    rm -f obj/lib/*.lexch; \
	    gprbuild -v -k \
	      -XLIBRARY_TYPE=$$kind \
	      -XBUILD_MODE=$(BUILD_MODE) \
	      -P $$proj -p -j$(PROCESSORS) ; \
	  done ; \
	done

.PHONY: bin
bin:
	which gprbuild
	which gcc
	for proj in $(BIN_PROJECTS) ; do \
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

install-lib:
	for proj in $(LIB_PROJECTS) ; do \
	  for kind in $(ALL_LIBRARY_TYPES) ; do \
	    gprinstall \
	      -XLIBRARY_TYPE=$$kind \
	      -XBUILD_MODE=$(BUILD_MODE) \
	      --prefix="$(DESTDIR)" \
	      --sources-subdir=include/$$(basename $$proj | cut -d. -f1) \
	      --build-name=$$kind \
	      --build-var=LIBRARY_TYPE --build-var=LAL_TOOLS_BUILD \
	      -P $$proj -p -f ; \
	  done ; \
	done

install-bin-strip:
	mkdir -p "$(DESTDIR)"
	cp -r bin "$(DESTDIR)/"
	# Don't strip debug builds
	test "$(BUILD_MODE)" = dev || strip "$(DESTDIR)/bin/"*


.PHONY: clean
clean:
	for proj in $(ALL_PROJECTS) $(BIN_PROJECTS) ; do \
		for build_mode in $(ALL_BUILD_MODES) ; do \
			for library_type in $(ALL_LIBRARY_TYPES) ; do \
				gprclean \
					-XLIBRARY_TYPE=$$library_type \
					-XBUILD_MODE=$$build_mode \
					-q -P $$proj; \
			done ; \
		done ; \
	done
