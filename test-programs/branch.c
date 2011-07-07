#include <stdbool.h>

bool b0(int x);
bool b1(int x);
bool b2(int x);

int f0();
int f1();
int f2();
int f3();

void branch_test(int c) {
  if (b0(c)) {
    f0();
  } else if (b1(c)) {
    f1();
  } else if (b2(c)) { 
    f2();
  } else {
    f3();
  }
}

void nested_for(unsigned m, unsigned n) {
  for (unsigned i = 0; i != m; ++i) {
    f0();
    for (unsigned j = 0; j != n; ++j) {
      f1();
    }
    f2();
  }
}

void return_inside_while() {
  int x = 0;
  while (b0(x)) {
    f0();
    if (b1(x)) return;
    f1();
    ++x;
  }
  f2();
}

int test_if_assign() {
  int x = f0();
  if (b0(0)) x = f1();
  int y = f2();
  if (b1(1)) y = f2();
  return x + y;
}
