int main()
{
    int c = malloc(8);
    int a = &c[0];
    int b = &*(c + 0);
    if (a == b && a == c)
        printf(1);
    else
        printf(-1);
}
