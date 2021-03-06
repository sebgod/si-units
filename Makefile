MMC=mmc
SUDO=sudo
MCFLAGS=--use-grade-subdirs -O3
MLLIBS=--ml generic_math --ml mercury_misc

SI_UNIT_SUBS := $(wildcard *.m)

.PHONY: test
test: test_si_units
	@for test_case in $^ ; do \
		./$$test_case ; \
	done

.PHONY: libsi_units
libsi_units: si_units.m $(SI_UNITS_SUBS)
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

test_si_units: libsi_units test_si_units.m
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

.PHONY: install
install: libsi_units
	@for lib in $^ ; do \
		$(SUDO) $(MMC) $(MCFLAGS) -m $@ $(MLLIBS) $$lib.install ; \
	done

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
