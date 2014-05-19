# See http://stackoverflow.com/questions/11775109/determine-if-makefile-is-executed-with-gmake
# and http://stackoverflow.com/questions/11775197/how-to-execute-gmake-make-from-a-bash-script-file
# for the GNUMAKE detection script
# If stock `make` is GNU Make, use `make`; otherwise use `gmake`
GNUMAKE=@`sh -c \
		'if (make --version | grep "^GNU Make" 2>&1 >/dev/null); \
		then echo make; else echo gmake; fi' 2>/dev/null`

TARGETMAKEFILE=	./Makefile.sfmt

all:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $@

.DEFAULT:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $@
