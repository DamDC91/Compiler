int hcf(int n1, int n2) {
    if (n2 != 0)
        return hcf(n2, n1 % n2);
    else
        return n1;
}

int main() {
    int n1, n2;
    n1 = scanf();
    n2 = scanf();
    printf(n1);
    printf(n2);
    printf(hcf(n1,n2));
    return 0;
}

