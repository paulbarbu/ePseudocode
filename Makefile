.PHONY: test lint clean

test:
	cabal test --show-details=always # --keep-tix-files
	hpc markup dist/hpc/vanilla/tix/epseudocode-0.1.0.0/epseudocode-0.1.0.0.tix --exclude={Tests.Evaluator,Tests.Data,Tests.Parser,Tests.Scope} --destdir=dist/markup --hpcdir=dist/hpc/vanilla/mix/tests > /dev/null
	chromium dist/markup/hpc_index.html &

lint:
	~/.cabal/bin/hlint . --report

clean:
	rm -rf .hpc
	rm -rf tests.tix
	cabal clean

