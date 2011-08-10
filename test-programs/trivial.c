#include <stdio.h>

int int32_add(int x, int y) {
  return (x + y);
}

int int32_mul(int x, int y) 
{
    return (x * y);
}

int int32_square(int x) 
{
    return int32_mul(x, x);
}

// int32_muladd(x,y) computes (x + y) * (x + y)
int int32_muladd(int x, int y) 
{
    int t = int32_add(x,y);
    int u = int32_square(t);
    return u;
}

int ptr_arith_sum(int* vals, int n)
{
    int sum = 0;
    for (int* p = vals; p != vals + n - 1; ++p)
        sum += *p;
    return sum;
}
