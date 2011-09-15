/* Example lss driver program for aes128BlockEncrypt. */

#include <inttypes.h>
#include <stdint.h>
#include <sym-api.h>
#include <stdio.h>
#include "aes128BlockEncrypt.h"

int main()
{
  SWord32 *pt  = lss_fresh_array_uint32(4, 0x8899aabbUL);
  SWord32 *key = lss_fresh_array_uint32(4, 0x08090a0bUL);
  SWord32 ct[4];

  aes128BlockEncrypt(pt, key, ct);

  lss_write_aiger_array_uint32(ct, 4, "aes.aig");

  printf("%08x\n", ct[0]);
  printf("%08x\n", ct[1]);
  printf("%08x\n", ct[2]);
  printf("%08x\n", ct[3]);
  return 0;
}
