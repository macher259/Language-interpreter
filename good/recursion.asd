void f1(int n) {
    if (n == 0)
        return;
    print("f1: ");
    printInt(n);

    if (n % 2 == 0)
        f2(n - 1);
    else
        f1(n - 1);
}

void f2(int n) {
    if (n == 0)
        return;
    print("f2: ");
    printInt(n);

    if (n % 2 == 1)
        f1(n - 1);
    else
        f2(n - 1);
}

void main() {
    f1(10);
}