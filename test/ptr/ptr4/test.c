int main()
{
    int a = 0, b = 0, c = &b, d = &a;
    *c = *d = 4;
    printf(*c);
}
