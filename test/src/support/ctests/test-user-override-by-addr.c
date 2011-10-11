#include <stdio.h>
#include <stdlib.h>
#include <sym-api.h>

int f1(int x)
{
  printf("f1\n");
  return x + 2;
}
int f2(int x)
{
  printf("f2\n");
  return x * 2;
}
int f3(int x)
{
  printf("f3\n");
  return x / 2;
}

int main()
{
  int r1 = f1(42);
  int r2 = f2(42);
  lss_override_function_by_addr(f1, f2);
  int r3 = f1(42);

  return (r3 == r2);
}
