ABC=../../abcBridge
LLVM_PRETTY=../../llvm-pretty
SAWCORE=../SAWCore

all: 
	cabal-dev install . ../Verinf $(SAWCORE) $(ABC) $(LLVM_PRETTY)

lss: 
	cabal-dev install . ../Verinf $(SAWCORE) $(ABC) $(LLVM_PRETTY) --flags='-build-library -build-tests -build-utils'

utils: 
	cabal-dev install . ../Verinf $(SAWCORE) $(ABC) $(LLVM_PRETTY) --flags='-build-lss -build-library -build-tests'

test: all
	dist/build/Tests/Tests

clean:
	cabal clean
	rm -f *.aig *.cnf
