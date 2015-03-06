/* Example lss driver program for aes128BlockEncrypt. */

#include <inttypes.h>
#include <stdint.h>
#include <sym-api.h>
#include <stdio.h>
#include "aes128BlockEncrypt.h"

int main()
{
  SWord32 *pt  = lss_fresh_array_uint32(4, 0x8899aabbUL, NULL);
  SWord32 *key = lss_fresh_array_uint32(4, 0x08090a0bUL, NULL);
  SWord32 ct[4];

  aes128BlockEncrypt(pt, key, ct);

  lss_write_aiger_array_uint32(ct, 4, "aes.aig");
  lss_write_cnf(!(pt[0] != ct[0] &&
                  pt[1] != ct[1] &&
                  pt[2] != ct[2] &&
                  pt[3] != ct[3]), "noleaks.cnf");

  return 0;
}
