void modify(string& s) {
    s = "HAPPY";
}

void dontModify(string s) {
    s = "ANGRY";
}

void main() {
    string str = "SAD";

    modify(str);

    print(str);
    // Should print "HAPPY".
    print("\n");

    dontModify(str);
    print(str);
    // "HAPPY"
}