ABC=../../abcBridge
LLVM_PRETTY=../../llvm-pretty

all: 
	cabal-dev install . ../Verinf $(ABC) $(LLVM_PRETTY)

lss: 
	cabal-dev install . ../Verinf $(ABC) $(LLVM_PRETTY) --flags='-build-library -build-tests -build-utils'

test: all
	dist/build/Tests/Tests
