int freshInt(int def);
void writeIntAiger(int v, char *file);
int evalIntAiger(int x, int input);
int main() {
    int x = freshInt(22);
    int y = x & 0x12345678;
    writeIntAiger(y, "test-fresh.aig");
    return evalIntAiger(y, 22);
}
