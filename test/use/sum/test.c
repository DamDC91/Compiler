// https://www.programiz.com/c-programming/examples/natural-number-sum-recursion

int addNumbers(int n) {
    if (n != 0)
        return n + addNumbers(n - 1);
    else
        return n;
}

int main() {
    int num;
    num = scanf();
    printf(addNumbers(num));
    return 0;
}

