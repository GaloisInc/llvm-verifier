#include <stdio.h>
#include <stdlib.h>
#include <sym-api.h>

int f1(int x) { return x + 2; }
int f2(int x) { return x * 2; }

int main()
{
  int r1 = f1(42);
  int r2 = f2(42);
  lss_override_function_by_name("f1", "f2");
  int r3 = f1(42);
  printf("r1 = %d, r2 = %d, r3 = %d\n", r1, r2, r3);
  return (r3 == r2);
}
