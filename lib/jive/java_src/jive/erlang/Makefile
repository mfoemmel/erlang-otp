# Dummy to run Makefile -T -f Makefile if clearmake is running.
TIME_FLAG := $(shell if [ X`basename $(MAKE)` = Xclearmake ]; then echo '-T'; fi)

default_target:
	$(MAKE) $(TIME_FLAG) -f Makefile.T

%:
	$(MAKE) $(TIME_FLAG) -f Makefile.T $@

