void main() {
    int x = 2;
    printInt(x);
    // x = 2;

    
    {
        int x = 3;
        printInt(x);
        // x = 3
    }

    printInt(x);
    // x = 2

    boolean x = true;
    printBool(x);
    // x = true
}