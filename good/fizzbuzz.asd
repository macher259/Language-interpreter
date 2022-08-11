void main() {
    int n = 15;

    int i = 1;

    while (i <= n) {
        if (i % 15 == 0) {
            print("Fizz Buzz\n");
        }
        if (i % 3 == 0 && i % 5 != 0) {
            print("Fizz\n");
        }
        if (i % 5 == 0 && i % 3 != 0) {
            print("Buzz\n");
        }
        if (i % 3 != 0 && i % 5 != 0) {
            printInt(i);
        }
        i++;        
    }
}