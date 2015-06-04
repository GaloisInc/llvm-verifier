#include <stdint.h>
#include <stdio.h>
#include "sym-api.h"

uint8_t test_case(uint8_t in) {
 switch (in) {
 case 0:
   return 1;
 case 1:
   return 2;
 case 2:
   return 3;
 case 3:
   return 4;
 case 4:
   return 5;
 case 5:
   return 6;
 case 6:
   return 7;

 default:
   return 42;
 }
}

const int test_vectors = 10;

/* Note: some 8-bit values given in least-significant digit first order.
 */
uint8_t input_vectors[8*test_vectors] =
  { 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 0,
    0, 1, 1, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 1, 0, 1,
    0, 1, 0, 1, 0, 1, 1, 0
  };

/* The expected outputs from evaluating on the above input vectors.
 */
uint8_t expected_output[test_vectors] =
  { 1,
    2,
    3,
    4,
    5,
    6,
    7,
    42,
    42,
    42
  };

int main ()
{
  uint8_t x = lss_fresh_uint8(9);
  uint8_t ret = test_case(x);

  /* Check all the test vectors */
  int correct = 1;
  for(int i=0; i<test_vectors; i++ ) {
    uint8_t eval = lss_eval_aiger_uint8( ret, input_vectors + (8*i), 8 );
    correct &= eval == expected_output[i];
  }

  return (correct ? 0 : 999);
}
