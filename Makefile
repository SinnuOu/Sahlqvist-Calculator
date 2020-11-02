.PHONY: target
target: app/Main.hs src/Lib.hs README.md stack.yaml package.yaml c_src/c_parseM.tab.c
	stack build
c_src/c_parseM.tab.c c_src/c_parseM.tab.h: c_src/c_parseM.y c_src/c_parseM.h c_src/lex.yy.h
	cd c_src; stack exec -- bison -d c_parseM.y
c_src/lex.yy.c c_src/lex.yy.h: c_src/c_parseM.l c_src/c_parseM.h
	cd c_src; stack exec -- flex --header-file=lex.yy.h c_parseM.l
.PHONY: clean
clean:
	cd c_src; stack exec -- rm -f lex.yy.h lex.yy.c c_parseM.tab.h c_parseM.tab.c
	stack exec -- rm -f stack.yaml.lock Sahlqvist-Calculator.cabal
	stack exec -- find . -name "*~" | stack exec -- xargs rm -rf
	stack exec -- rm -rf .stack-work/