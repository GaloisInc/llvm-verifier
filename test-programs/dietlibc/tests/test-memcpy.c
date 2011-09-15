#include <stdio.h>
#include <string.h>
int main() {
    char s[4] = "foo";
    char t[4];
    memcpy(t, s, 4);
    printf("%s\n", t);
    return 0;
}
