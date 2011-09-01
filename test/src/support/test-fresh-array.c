#define SIZE 10
int *fresh_uint32_array(int size, int def);
void write_uint32_array_aiger(int *v, int size, char *file);
int main() {
    int *x = fresh_uint32_array(SIZE, 22);
    int i;
    for(i = 0; i < SIZE; i++) {
        x[i] &= 0x01;
    }
    write_uint32_array_aiger(x, SIZE, "test-fresh-array.aig");
    return 0;
}
