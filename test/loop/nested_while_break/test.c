int main() {
    int i,j,res;
    i=res=0;
    while(i < 4)
    {
        i = i + 1;
        j = 0;

        while (j < 30)
        {
            j = j + 1;
            res = res + 1;
            if (j == 12)
                break;
        }

    }
    printf(res);
}
