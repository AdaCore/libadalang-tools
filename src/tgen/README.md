# Type GENeration library (TGen)

This library is being developed as part of the TAGAda research project.
The library contains three parts, a type translation and support library
generation part that is included in the laltools library, a runtime
component (that mis used by the support library when generated), and some
template files that are used to generate the support library.

The runtime library, called tgen_rts is located in the corresponding subdirectory
and is copied as part of the installation of the laltools by calling
`make install-tgen`, under `<installation_prefix>/share/tgen/tgen_rts`.
Once a support library has been generated, the user should either build and
install tgen_rts.gpr or at least add the path to the project in his
`GPR_PROJECT_PATH`.

Likewise, the templates are installed under `<install_prefix>/share/tgen/templates`
and tools invoking the generation part (i.e. the subprograms in TGen.LibGen) should
use this directory when creating the generation context. See
`testsuite/ada_drivers/gen_marshalling_lib` and `testsuite/tests/test/tagada_marshalling`
for examples on how this can be achieved.