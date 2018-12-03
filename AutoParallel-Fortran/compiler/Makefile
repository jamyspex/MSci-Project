all: AutoParallel-Fortran

profile: *.hs ../language-fortran/src/Language/Fortran.hs
	ghc -rtsopts -O2 Transformer.hs -o AutoParallel-Fortran -i../language-fortran/src/ +RTS -h -RTS

AutoParallel-Fortran: *.hs ../language-fortran/src/Language/Fortran.hs
#	ghc -Wall -O2 Main.hs -o AutoParallel-Fortran -i../language-fortran/src/
	ghc -fwarn-unused-binds -O2 Main.hs -o AutoParallel-Fortran -i../language-fortran/src/

clean:
	rm -f *.hi *.o AutoParallel-Fortran
