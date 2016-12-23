# To build in production mode, do "make BUILD_MODE=prod".
# To build in development mode, do "make BUILD_MODE=dev" or just "make".

BUILD_MODE = dev
PROCESSORS = 0

.PHONY: all
all:
	which gprbuild
	which gcc
	gprbuild -v -k -XLIBRARY_TYPE=static -XBUILD_MODE=${BUILD_MODE} -P src/build.gpr -p -j${PROCESSORS}
#???	LIBRARY_TYPE=relocatable gprbuild -v -P src/build.gpr -p
	cd bin ; rm -f gnatmetric gnatpp
	cd bin ; cp -p lalmetric gnatmetric
	cd bin ; cp -p lalpp gnatpp

#???It's not clear to me why LIBRARY_TYPE=relocatable is needed.

.PHONY: clean
clean:
	rm -rf obj bin
