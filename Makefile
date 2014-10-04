.PHONY: test lint clean

test:
	cabal test --show-details=always
	hpc markup dist/hpc/tix/epseudocode-0.1.0.0/epseudocode-0.1.0.0.tix --exclude={Tests.Parser,Tests.Scope} --destdir=dist/markup > /dev/null
	chromium dist/markup/hpc_index.html &

lint:
	hlint . --report

clean:
	rm -rf .hpc
	cabal clean

