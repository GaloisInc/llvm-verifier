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

int arr1() 
{
    int arr[1];
    arr[0] = 42;
    return arr[0];
}

int arr2() 
{
    int arr[3];
    arr[0] = 42;
    arr[1] = 99;
    arr[2] = arr[0] + arr[1];
    return arr[2];
}

int ptr_arith_sum(int* vals, int n)
{
    int sum = 0;
    for (int* p = vals; p != vals + n - 1; ++p)
        sum += *p;
    return sum;
}

int onedim_init()
{
    int a[4] = { 0, 1, 2, 3};
    return a[3];
}

typedef struct A
{ int x;
  char y;
} A;

A struct_test() 
{
    A a;
    a.x = 42;
    a.y = 'q';

    A b = { .x = 99, .y = 'z' };
    b.x = a.x;
    
    return b;
}

#if 0
int twodim()
{
    int a[4][4] = {
      {  0,  1,  2,  3 },
      {  4,  5,  6,  7 },
      {  8,  9, 10, 11 },
      { 12, 13, 14, 15 }
    };
    return (a[3][3] + a[1][2]); /* == 21 */
}

int matrix_mul_4x4()
{
  int a[4][4] = {
    { 0, 1, 2, 3     },
    { 4, 5, 6, 7     },
    { 8, 9,10, 11    },
    { 12, 13, 14, 15 },
  };
  int b[4][4] = {
    { 15, 14, 13, 12 },
    { 11, 10,  9,  8 },
    {  7,  6,  5,  4 },
    {  3,  2,  1,  0 },
  };
  int c[4][4];

  for(int i = 0; i < 4; ++i) {
    for (int j = 0; j < 4; ++j) {
      c[i][j] = 0;
      for (int k = 0; k < 4; ++k) {
        c[i][j] += a[i][k] * b[k][j];
      }
    }
  }

  return c[3][3];
}
#endif
