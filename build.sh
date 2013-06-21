set -e

if [ ! -e ./cabal-dev/bin/alex ] ; then
  cabal-dev install alex
fi

if [ ! -e ./cabal-dev/bin/happy ] ; then
  cabal-dev install happy
fi

if [ ! -e ./cabal-dev/bin/c2hs ] ; then
  cabal-dev install c2hs
fi

cabal --version
cabal-dev --version
ghc --version
ghc-pkg --version
alex --version
happy --version

if [ ! -e ./build ] ; then
  mkdir -p ./build
fi

if [ ! -e ./build/abcBridge ] ; then
  git clone ssh://builder@src.galois.com/abcBridge ./build/abcBridge
fi

if [ ! -e ./build/llvm-pretty ] ; then
  git clone ssh://builder@src.galois.com/llvm-pretty ./build/llvm-pretty
fi

cabal-dev install --force-reinstalls ./build/llvm-pretty
cabal-dev install --force-reinstalls ./build/abcBridge
cabal-dev install --force-reinstalls ../Verinf
cabal-dev install --force-reinstalls ../SAWCore
cabal-dev install --enable-tests
