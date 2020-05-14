# If stock `make` is GNU Make, use `make`; otherwise use `gmake`
GNUMAKE=@`sh -c 'gmakebin=\`which gmake\`; if [ ! -z $$gmakebin] && [-x $$gmakebin ]; then echo gmake; else echo make; fi'`

TARGETMAKEFILE=	./Makefile.sfmt

all:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $@

.DEFAULT:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $@
