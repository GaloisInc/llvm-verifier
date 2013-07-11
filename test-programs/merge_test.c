#include <stdio.h>
int main() {
    int x, i;
    x = 0;
    for (i = 0; i < 10; i++) {
        x += i;
    }
    printf("%d\n", x);
    return 0;
}
