int f(int & a, int &b, int c) {
    return a + b + c;
}

int g() {
    int x = 3;

    return x;
}

void main() {
    f(g(), g(), g());
}