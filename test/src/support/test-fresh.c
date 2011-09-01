int fresh_uint32(int def);
void write_uint32_aiger(int v, char *file);
int eval_uint32_aiger(int x, int input);
int main() {
    int x = fresh_uint32(22);
    int y = x & 0x12345678;
    write_uint32_aiger(y, "test-fresh.aig");
    return eval_uint32_aiger(y, 22);
}
