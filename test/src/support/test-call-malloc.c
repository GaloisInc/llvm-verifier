#include <stdlib.h>

int main() {
    int *a = malloc(sizeof(int));
    *a = 34289;
    return *a;
}
