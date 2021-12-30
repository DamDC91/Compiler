// https://www.programiz.com/c-programming/examples/factors-number

int main() {
    int num, i;
    num = scanf();
    printf(num);
    for (i = 1; i <= num; i=i+1) {
        if (num % i == 0) {
            printf(i);
        }
    }
    return 0;
}
