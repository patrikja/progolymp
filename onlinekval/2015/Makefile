
GHC = ghc -O2 -ferror-spans -threaded -rtsopts
GHCPROF = -prof -fprof-auto

%.prof:
	${GHC} ${GHCPROF} $*.hs -o $@
