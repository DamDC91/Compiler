int main()
{
    int a = 0, b = 0, c = &b, d = &c;
    *(*d + 1) = 10;
    printf(a);
}
