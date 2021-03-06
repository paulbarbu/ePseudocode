.PHONY: test lint clean

run:
	rm -rf epseudocode.tix
	cabal run

test:
	rm -rf epseudocode.tix
	cabal build
	./test/files.sh
	cabal test --show-details=always # --keep-tix-files
	hpc markup dist/hpc/vanilla/tix/epseudocode-0.4.0.0/epseudocode-0.4.0.0.tix --exclude={Tests.Evaluator,Tests.Data,Tests.Parser,Tests.Scope} --destdir=dist/markup --hpcdir=dist/hpc/vanilla/mix/tests > /dev/null
	firefox dist/markup/hpc_index.html &

lint:
	~/.cabal/bin/hlint . --report

clean:
	rm -rf .hpc
	rm -rf tests.tix
	rm -rf epseudocode.tix
	cabal clean
	cabal configure --enable-tests --enable-coverage
