int main() {
    int a;
    a= 32;
    if (a > 0) 
    {
        if (a > 20)
        {
            printf(1);
            if (a > 30)
            {
                if (a > 40)
                {
                    printf(2);
                }
                else if (a == 31)
                {
                    printf(3);
                }
                else
                {
                    printf(4);
                }
            }
            else
            {
                printf(4);
            }
        }
        printf(6);
    }
    printf(7);
}
