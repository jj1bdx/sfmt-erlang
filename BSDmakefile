# for BSD Make (BSDmakefile takes precedence over Makefile)
# passing all arguments sequentially to gmake

.PHONY: ${.TARGETS}

all:
	@gmake all

.DEFAULT:
	@gmake ${.TARGET}
