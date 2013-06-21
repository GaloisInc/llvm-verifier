set -e

bash build.sh

./dist/build/test-llvm/test-llvm

bash stage.sh
