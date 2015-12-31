GHC = ghc
GHCFLAGS = -O2 -ferror-spans -threaded -rtsopts

towers: Main.hs
	${GHC} ${GHCFLAGS} Main.hs -o towers

run: towers
	./towers +RTS -M1024m -RTS < sample1.in
