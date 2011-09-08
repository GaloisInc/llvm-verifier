#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/aes.h>
#include "sym-api.h"

void printBlock(const unsigned char out[16]) {
  unsigned i;
  for (i = 0; i != 16; ++i) {
    printf("%02x", out[i]);
  }
}

void dummy_memset(unsigned char *buf, unsigned char v, unsigned char sz) {
  int i;
  for(i = 0; i < sz; i++)
    buf[i] = v;
}

int main(int argc, char** argv) {

  //unsigned char userKey[16];
  //unsigned char in[16];
  AES_KEY key;
  unsigned char out[16];

  //dummy_memset(userKey, 0, 16);
  //userKey[15] = 1;
  //dummy_memset(in, 0, 16);
  //in[15] = 2;

  unsigned char *in = lss_fresh_array_uint8(16, 0);
  unsigned char *userKey = lss_fresh_array_uint8(16, 0);

  AES_set_encrypt_key(userKey, 128, &key);
  AES_encrypt(in, out, &key);
  printBlock(out);
  lss_write_aiger_array_uint8(out, 16, "aes.aig");
  printf("\n");
  return 0;
}
