int f(int & a, int &b, int c) {
    return a + b + c;
}

void main() {
    f(2,3, 3);
}