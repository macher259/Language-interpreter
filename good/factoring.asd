void factor(int n) {
    int i = 1;
    while (i <= n) {
        if (n % i == 0)
            printInt(i);
            i++;
    }
}

void main() {
    factor(32);
}