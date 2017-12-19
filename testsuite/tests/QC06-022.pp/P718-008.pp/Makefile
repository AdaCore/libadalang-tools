GNATMAKE         = gnatmake
GNATMAKEFLAGS    =-q
GNATPP           = gnatpp

GNATPPFLAGS      =--output=normalized_compact_array_type_declaration.adb.pp \
    --vertical-enum-types \
    --vertical-array-types \
    --vertical-named-aggregates \
    --vertical-case-alternatives \
                     --based-grouping=4 \
                   --call_threshold=0 \
                   --decimal-grouping=3 \
                   --eol=unix \
                   --insert-blank-lines \
                   --par_threshold=0 \
                   --separate-loop-then \
                   --split-line-before-op \
                   --use-on-new-line \
                   -M132 \
                   -cl3

EXTRAGNATPPFLAGS = -nM

PROGRAMS = gnatpp_playground is_separation compact_array_type_declaration

PROGRAMS.prepare = $(addsuffix .prepare, $(PROGRAMS))
PROGRAMS.gnatpp  = $(addsuffix .gnatpp, $(PROGRAMS))

all: $(PROGRAMS.gnatpp)

ada_normalize:
	$(GNATMAKE) $(GNATMAKEFLAGS) -o ada_normalize normalize

$(PROGRAMS.prepare): ada_normalize
	$(GNATMAKE) $(GNATMAKEFLAGS) $(basename $@).adb
	./ada_normalize $(basename $@).adb > normalized_$(basename $@).adb

$(PROGRAMS.gnatpp): %.gnatpp : %.prepare
	$(GNATPP) $(GNATPPFLAGS) $(EXTRAGNATPPFLAGS) normalized_$(basename $@).adb
	diff -u $(basename $@).adb normalized_$(basename $@).adb.pp

clean:
	rm -f *.o *.ali $(PROGRAMS) ada_normalize normalized_*
