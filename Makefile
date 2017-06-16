# To build in production mode, do  "make LIBRARY_TYPE=static BUILD_MODE=prod".
# To build in development mode, do "make LIBRARY_TYPE=static BUILD_MODE=dev".

BUILD_MODE = dev
LIBRARY_TYPE = static
PROCESSORS = 0

.PHONY: all
all:
	which gprbuild
	which gcc
	gprbuild -v -k -XLIBRARY_TYPE=${LIBRARY_TYPE} -XXMLADA_BUILD=${LIBRARY_TYPE} \
		-XBUILD_MODE=${BUILD_MODE} -P src/build.gpr -p -j${PROCESSORS}
	cd bin && rm -f gnatmetric gnatpp gnatstub && \
	    cp -p lalmetric gnatmetric && \
	    cp -p lalpp gnatpp && \
	    cp -p lalstub gnatstub

.PHONY: clean
clean:
	rm -rf obj bin
