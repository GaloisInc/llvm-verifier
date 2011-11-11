#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/sha.h>

void printBlock(const unsigned char* out, size_t len) {
  unsigned i;
  for (i = 0; i != len; ++i) {
    printf("%02x", out[i]);
  }
}

int main(int argc, char** argv) {

  size_t dataSize = 10;
  unsigned char data[dataSize];
  memset(data, 0, dataSize);

  // Store key.
  size_t mdSize = SHA384_DIGEST_LENGTH;
  unsigned char md[mdSize];

  SHA512_CTX c;
  SHA384_Init(&c);
  SHA384_Update(&c, data, dataSize);
  SHA384_Final(md, &c);

  printBlock(md, mdSize);
  printf("\n");
  return 0;
}
