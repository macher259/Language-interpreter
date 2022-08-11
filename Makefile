interpreter:
	bnfc -m --functor -o generated grammar.cf 
	make -C generated
	ghc -i.:generated Main.hs -o interpreter
clean:
	rm -f *.o *.hi interpreter
	make -C generated distclean
	