#!/bin/sh

TARGET=lss-0.2b

NM=`uname`

mkdir -p ${TARGET}/bin
mkdir -p ${TARGET}/doc
mkdir -p ${TARGET}/tutorial
mkdir -p ${TARGET}/test-programs

if [ "${OS}" == "Windows_NT" ]; then
  EXEDIR=windows
elif [ "${NM}" == "Darwin" ]; then
  EXEDIR=macosx  
else
  EXEDIR=linux
fi

echo Staging ...

cp RELEASE_README                              ${TARGET}/README
cp ABC_LICENSE                                 ${TARGET}
cp doc/lss-usage.txt                           ${TARGET}/doc
cp doc/lss-tutorial/lss-tutorial.pdf           ${TARGET}/tutorial
cp -r doc/lss-tutorial/code                    ${TARGET}/tutorial/code
cp dist/build/lss/lss                          ${TARGET}/bin
cp -r test-programs/libgcrypt                  ${TARGET}/test-programs
cp -r sym-api                                  ${TARGET}/sym-api

sed -e 's/TOP=\.\.\/\.\.\/\.\./TOP=\.\.\/\.\./' ${TARGET}/tutorial/code/Makefile > t
mv t ${TARGET}/tutorial/code/Makefile
sed -e 's/cabal-dev\///' ${TARGET}/test-programs/libgcrypt/sha384/Makefile > t
mv t ${TARGET}/test-programs/libgcrypt/sha384/Makefile

if [ "${OS}" == "Windows_NT" ]; then
  zip -r ${TARGET}-${EXEDIR}.zip ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.zip"
else
  tar cvfz ${TARGET}-${EXEDIR}.tar.gz ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.tar.gz"
fi
