MMC=mmc
MCFLAGS=--use-grade-subdirs
MLLIBS=--ml generic_math

SI_UNIT_SUBS := $(wildcard si_units.*.m)

.PHONY: test
test: test_si_units
	@./$<

.PHONY: libsi_units
libsi_units: si_units.m $(SI_UNITS_SUBS)
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

test_si_units: libsi_units test_si_units.m
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

.PHONY: clean
clean:
	rm -f *.init
	rm -f *.mh
	rm -f *.err
	rm -f *.a
	rm -f *.so
	rm -f *.dylib
	rm -f *.jar
	rm -f *.beams
	rm -f test_si_units

.PHONY: realclean
realclean: clean
	rm -fR Mercury/
