# To build in production mode, do "make BUILD_MODE=prod LIBRARY_TYPE=relocatable".
# To build in development mode, do "make BUILD_MODE=dev LIBRARY_TYPE=static".

BUILD_MODE = dev
PROCESSORS = 0
LIBRARY_TYPE = relocatable

.PHONY: all
all:
	which gprbuild
	which gcc
	gprbuild -v -k -XLIBRARY_TYPE=${LIBRARY_TYPE} -XXMLADA_BUILD=${LIBRARY_TYPE} \
		-XBUILD_MODE=${BUILD_MODE} -P src/build.gpr -p -j${PROCESSORS}
	cd bin && rm -f gnatmetric gnatpp gnatstub && cp -p lalmetric gnatmetric && cp -p lalpp gnatpp && cp -p lalstub gnatstub

.PHONY: clean
clean:
	rm -rf obj bin
