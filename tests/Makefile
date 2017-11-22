all:
	ghc -Wall -fwarn-tabs -rtsopts -fno-warn-orphans --make bm.hs
	ghc -Wall -fwarn-tabs -rtsopts -fno-warn-orphans --make qc.hs

clean:
	rm -f *.hi *.o
	rm -f Sound/OSC/*.hi Sound/OSC/*.o
	rm -f bm qc

pkg-dep:
	cabal install test-framework-quickcheck2
	cabal install criterion
