#define SIZE 10
int *fresh_array_uint32(int size, int def);
void write_aiger_array_uint32(int *v, int size, char *file);
int main() {
    int *x = fresh_array_uint32(SIZE, 22);
    int i;
    for(i = 0; i < SIZE; i++) {
        x[i] &= 0x01;
    }
    write_aiger_array_uint32(x, SIZE, "test-fresh-array.aig");
    return 0;
}
