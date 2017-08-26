Libadalang-tools
================

This repository hosts sources for several Libadalang-based development tools
for Ada:

* `lalpp`, a pretty-printer to reformat source code.

* `lalmetric`, which can compute various source code metrics, such as SLOC
  count.

* `lalstub`, which creates body stub (empty but compilable bodies) for library
  unit declarations.

These are currently under development.


Build
-----

In order to build these tools, first build and install
[Libadalang](https://github.com/AdaCore/libadalang/). Then, just run from this
directory:

```sh
make
```

This will build the various tools as statically linked programs in the `bin`
directory.


Testing
-------

The testsuite framework depends on
[GNATpython](https://github.com/Nikokrock/gnatpython): please install it first.
Then run:

```sh
testsuite/testsuite.py
```

This will run all testcases and display a summary of passed/failed tests.
