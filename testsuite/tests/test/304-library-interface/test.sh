gnattest -P prj_2.gpr --stub -q

# We only check for correct compilation of the sources, if the interface is not
# correctly specified, gprbuild will stop the compilation.
make --quiet -C obj_2/gnattest_stub/harness BUILDERFLAGS=-q
