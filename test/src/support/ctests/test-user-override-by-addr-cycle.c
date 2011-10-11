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

int cycle()
{
    lss_override_function_by_addr(f2, f1);
    int r1 = f1(42); /* calls f2() */
    int r2 = f2(42); /* calls f1() */
    return (r1 == 84 && r2 == 44);
}

int main()
{
  int r1 = f1(42);
  int r2 = f2(42);
  lss_override_function_by_addr(f1, f2);
  lss_override_function_by_addr(f2, f3);
  /* At this point, f1 should bind to f2, not transitively to f3.
     Otherwise, cycles would be permissible and we don't want that. */
  int r3 = f1(42);
  int r4 = f2(42);
  int r5 = f3(42);
  return (r3 == r2 && r4 == r5 && cycle());
}
