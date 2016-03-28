.PHONY: all
all:
	LIBRARY_TYPE=relocatable gprbuild -P src/build.gpr -p

#???It's not clear to me why LIBRARY_TYPE=relocatable is needed.

.PHONY: clean
clean:
	rm -rf obj bin
