# for BSD Make (BSDmakefile takes precedence over Makefile)
# passing all arguments sequentially to gmake

.PHONY: ${.TARGETS}

.for CMD in ${.TARGETS}
${CMD}:
	@gmake ${CMD}
.endfor
