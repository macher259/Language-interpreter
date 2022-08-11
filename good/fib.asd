int fact(int n) {
    if (n < 2) {
        return 1;
    } 
    return fact(n-1) * n;
}

int fact2(int n) {
    int i = 1;
    int val = 1;
    while (i <= n) {
        val = val * i;
        i++;
    }
    return val;
}

void main() {
    int a = fact2(10);
    int b = fact(10);


    printInt(a);
    printInt(b);
}