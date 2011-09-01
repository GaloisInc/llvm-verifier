#include <stdlib.h>

int main() {
    int *a = alloca(sizeof(int));
    *a = 34289;
    return *a;
}
