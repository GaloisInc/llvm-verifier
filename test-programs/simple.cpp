class C {
    private:
      int x, y;
    public:
      C(int a, int b) {
          x = a;
          y = b;
      }
      int get_x() { return x; }
      int get_y() { return y; }
};

int main() {
    C *c = new C(2, 0);
    return c->get_y();
    //C c(2, 0);
    //return c.get_x();
}
