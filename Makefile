ABC=../../abcBridge
LLVM_PRETTY=../../llvm-pretty

all: 
	cabal-dev install . ../Verinf $(ABC) $(LLVM_PRETTY) --flags='abc-backend build-tests'

test: all
	dist/build/Tests/Tests

