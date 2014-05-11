MMC=mmc
MCFLAGS=--use-grade-subdirs --infer-all
MLLIBS=--ml generic_math

.PHONY: test
test: si_units
	@./$<

si_units: si_units.m
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

