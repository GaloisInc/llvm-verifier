#include <stdlib.h>
#include <stdbool.h>

bool b0(int x);
bool b1(int x);
bool b2(int x);

int f0();
int f1();
int f2();

void multiexit_test(int c)
{
  if (b0(c)) {
    f0();
  }
  else if (b1(c)) {
    f1();
    exit(1);
  }
  else {
    f2();
  }
}

