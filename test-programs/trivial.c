#include <stdio.h>

int int32_add(int x, int y) {
  return (x + y);
}

int ptr_arith_sum(int* vals, int n)
{
    int sum = 0;
    for (int* p = vals; p != vals + n - 1; ++p)
        sum += *p;
    return sum;
}
