.PHONY: all
all:
#???	gprbuild -v -P src/build.gpr -p
	gprbuild -v -k -XLIBRARY_TYPE=static -P src/build.gpr -p
#???	LIBRARY_TYPE=relocatable gprbuild -v -P src/build.gpr -p
	cd bin ; cp -p lalmetric gnatmetric
	cd bin ; cp -p lalpp gnatpp

#???It's not clear to me why LIBRARY_TYPE=relocatable is needed.

.PHONY: clean
clean:
	rm -rf obj bin
