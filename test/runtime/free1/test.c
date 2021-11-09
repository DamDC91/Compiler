int main()
{
    int a = malloc(12);
    printf(a[-1]);
    printf(a[12]);
    free(a);
    printf(a[-1]);
    printf(a[12]);
}
