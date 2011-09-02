int fresh_uint32(int def);
void write_aiger_uint32(int v, char *file);
int eval_aiger_uint32(int x, int input);
int main() {
    int x = fresh_uint32(22);
    int y = x & 0x12345678;
    write_aiger_uint32(y, "test-fresh.aig");
    return eval_aiger_uint32(y, 22);
}
