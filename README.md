Libadalang-tools
================

This repository hosts sources for several Libadalang-based development tools
for Ada:

* `gnatpp`, a pretty-printer to reformat source code.

* `gnatmetric`, which can compute various source code metrics, such as SLOC
  count.

* `gnatstub`, which creates body stub (empty but compilable bodies) for library
  unit declarations.

* `gnattest`, which creates creates unit-test skeletons
  as well as a test driver infrastructure.

These are currently under development.

Installing
----------
If you are only concerned in getting the suite of executables you can most easily do so by using the [Alire][alire] package manager to handle the building and installing for you. The steps are:
```bash
alr install libadalang_tools
```
The build step will require a substantial amount of CPU and RAM, but once successful you should find the executables in `$HOME/.alire/bin`. If this is not in the `$PATH` set up by your shell's resource files, you should do so now, typically doing something like this (example is using Bash):
```bash
echo 'export PATH="$HOME/.alire/bin:$PATH"' >> ~/.bashrc
```

Note that the crates on Alire are not always up to date with the [latest released version][releases].

[alire]: https://alire.ada.dev/
[releases]: https://github.com/AdaCore/libadalang-tools/releases

Build
-----

In order to build these tools, first build and install
[Libadalang](https://github.com/AdaCore/libadalang/). Note that in order to
build this repository's `master` branch, you should use Libadalang and
Langkit's `stable` branches. Then, just run from this directory:

```sh
make
```

This will build the various tools as statically linked programs in the `bin`
directory.


Testing
-------

The testsuite framework depends on
[GNATpython](https://github.com/Nikokrock/gnatpython): please install it first.
Put the `bin` subdirectory on your `PATH`.  Then run:

```sh
testsuite/testsuite.py
```

This will run all testcases and display a summary of passed/failed tests. See
[testsuite/README.rst](the testsuite README) for more detailed instructions.


License
=======

All source files in this repository are licensed under the terms of the GNU
General Public License version 3 (GPLv3). See the [LICENSE](LICENSE) file for
more information.
