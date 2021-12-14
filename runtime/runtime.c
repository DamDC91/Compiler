
int pow(int nb, int exp)
{
    int i, res;
    res = 1;
    for (i = 0; i < exp; i = i + 1)
        res=res*nb;
    return res;
}

int local_printf(int nb)
{
   if (nb != 0)
   {
        int c;
        c = nb % 10;
        nb = nb /10;
        local_printf(nb);
        putchar(c + 48);
   }
}

int printf(int nb)
{
    if(nb == 0) {
        putchar(48);
    }
    else if(nb < 0) {
        putchar(45);
        local_printf(-nb);
    }
    else {
        local_printf(nb);
    }
    putchar(10);
}

int scanf()
{
    int c, res = 0;
    int sign = 1;
    c = getchar();
    if (c == 45)
    {
        sign = -1;
        c = getchar();
    }
    while(c != 10)
    {
        res = res*10 + (c - 48);
        c = getchar();
    }
    return sign*res;
}

int abs(int nb)
{
    if (nb >= 0)
        return nb;
    else
        return -nb;
}

int malloc(int s)
{
    // min s = 2
    // negative size = used block
    // positive size = free block
    if (s < 2)
        s = 2;

    int firstFree = *0; 
    do {
        int blockSize = firstFree[-1];
        if (blockSize >= s) 
        {
            if (blockSize != firstFree[blockSize])
            {
                printf(-1);
                int err = 1/0;
                //Error TODO
            }
            if (blockSize <= s + 4) //  the remaining size is tiny, no spliting
            {
                firstFree[-1] = -blockSize;
                firstFree[blockSize] = -blockSize; 

                int LeftBlockPtr = firstFree[0];
                int RightBlockPtr = firstFree[blockSize-1];
                if(LeftBlockPtr)
                {
                    int LeftBlockSize = LeftBlockPtr[-1];
                    LeftBlockPtr[LeftBlockSize - 1] = RightBlockPtr;
                }
                else
                    *0 = RightBlockPtr;
                if(RightBlockPtr)
                    RightBlockPtr[0] = LeftBlockPtr;
                return firstFree; 
            }
            else // the remaining size is significant, spiting the block in 2
            {
                int size = blockSize - s - 2;
                firstFree[-1] = -s;
                firstFree[s] = -s;

                int newBlockPtr = firstFree + s + 2;
                newBlockPtr[size] = size;
                newBlockPtr[-1] = size;
                int LeftBlockPtr = firstFree[0];
                int RightBlockPtr = firstFree[blockSize - 1];

                if(LeftBlockPtr)
                {
                    int LeftBlockSize = LeftBlockPtr[-1];
                    LeftBlockPtr[LeftBlockSize - 1] = newBlockPtr;
                }
                else
                    *0 = newBlockPtr;

                if(RightBlockPtr)
                {
                    RightBlockPtr[0] = newBlockPtr;
                }
                return firstFree;
            }
        }
        firstFree = firstFree[-blockSize -1]; // blocksize is negative
    } while (firstFree != 0);
    return 0;
}

int free(int p)
{
    if (p==0)
        return 0;

    int size = -p[-1]; 

    if (p[-1] >= 0 || p[-1] != p[abs(size)])
        int err = 1/0;
        //Error TODO
        

    p[size] = size;
    p[-1] = size;

    p[0] = 0;
    p[size - 1] = *0;

    **0 = p;
    *0 = p;
}

int start2() 
{
    *0 = *0 + 1;
    **0 = 0;
    *(*0 + 4096 - 1) = 0;
    *(*0 - 1) = 4096;
    *(*0 + 4096) = 4096;
    return 0;
}
