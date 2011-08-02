#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/aes.h>

void printBlock(const unsigned char out[16]) {
  unsigned i;
  for (i = 0; i != 16; ++i) {
    printf("%02x", out[i]);
  }
}

int main(int argc, char** argv) {

  unsigned char userKey[16];
  unsigned char in[16];
  AES_KEY key;
  unsigned char out[16];

  memset(userKey, 0, 16);
  userKey[15] = 1;
  memset(in, 0, 16);
  in[15] = 2;

  AES_set_encrypt_key(userKey, 128, &key);
  AES_encrypt(in, out, &key);
  printBlock(out);
  printf("\n");
  return 0;
}
