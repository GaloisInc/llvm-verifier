#!/bin/sh

TARGET=lss-alpha-rc1

NM=`uname`

mkdir -p ${TARGET}/bin
mkdir -p ${TARGET}/doc
mkdir -p ${TARGET}/tutorial

if [ "${OS}" == "Windows_NT" ]; then
  EXEDIR=windows
  cp cabal-dev/bin/abc.dll ${TARGET}/bin
elif [ "${NM}" == "Darwin" ]; then
  EXEDIR=macosx  
else
  EXEDIR=linux
fi

echo Staging ...

cp RELEASE_README                              ${TARGET}/README
cp ABC_LICENSE                                 ${TARGET}
cp doc/lss-usage.txt                           ${TARGET}/doc
cp doc/lss-tutorial/out/lss-tutorial.pdf       ${TARGET}/tutorial
cp -r doc/lss-tutorial/code                    ${TARGET}/tutorial/code
cp dist/build/lss/lss                          ${TARGET}/bin

if [ "${OS}" == "Windows_NT" ]; then
  zip -r ${TARGET}-${EXEDIR}.zip ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.zip"
else
  tar cvfz ${TARGET}-${EXEDIR}.tar.gz ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.tar.gz"
fi
