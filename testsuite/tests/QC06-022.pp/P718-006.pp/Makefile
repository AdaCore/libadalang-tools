GNATMAKE         = gnatmake
GNATMAKEFLAGS    =-q
GNATPP           = gnatpp

GNATPPFLAGS      =--vertical-enum-types \
    --vertical-array-types \
    --vertical-named-aggregates \
    --vertical-case-alternatives \
                     -M132 \
                   -cl3 \
                   --separate-loop-then \
                   --use-on-new-line \
                   --par_threshold=0 \
                   --call_threshold=0 \
                   --insert-blank-lines \
                   --eol=unix \
                   --based-grouping=3 \
                   --decimal-grouping=3 \
                   --split-line-before-op

EXTRAGNATPPFLAGS = -nM

all: prepare
	$(GNATPP) $(GNATPPFLAGS) $(EXTRAGNATPPFLAGS) normalized_gnatpp_playground.adb
	diff -u gnatpp_playground.adb normalized_gnatpp_playground.adb.pp

ada_normalize:
	$(GNATMAKE) $(GNATMAKEFLAGS) -o ada_normalize normalize

prepare: ada_normalize
	$(GNATMAKE) $(GNATMAKEFLAGS) gnatpp_playground.adb
	./ada_normalize gnatpp_playground.adb > normalized_gnatpp_playground.adb

clean:
	rm -f *.o *.ali gnatpp_playground ada_normalize normalized_gnatpp_playground.adb*
