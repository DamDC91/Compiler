
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
    // TODO handle negavtive nb
    int c, res = 0;
    c = getchar();
    while(c != 10)
    {
        res = res*10 + (c - 48);
        c = getchar();
    }
    return res;
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
    //putchar(70);
    //printf(firstFree);
    do {
        int blockSize = *(firstFree-1);
        /*putchar(70);
        printf(firstFree);
        putchar(83);
        printf(blockSize);*/
        if (blockSize >= s) 
        {
            if (blockSize != *(firstFree + blockSize))
            {
                printf(-1);
                int err = 1/0;
                //Error TODO
            }
            if (blockSize <= s + 4) //  the remaining size is tiny, no spliting
            {
                *(firstFree - 1) = -blockSize;
                *(firstFree + blockSize) = -blockSize; 

                int LeftBlockPtr = firstFree;
                int LeftBlockSize = *(LeftBlockPtr-1);
                int RightBlockPtr = firstFree + blockSize;
                if(LeftBlockPtr)
                    *(LeftBlockPtr + LeftBlockSize - 1) = RightBlockPtr;
                if(RightBlockPtr)
                    *(RightBlockPtr) = LeftBlockPtr;
                return firstFree; 
            }
            else // the remaining size is significant, spiting the block in 2
            {
                int size = blockSize - s - 2;
                *(firstFree - 1) = -s;
                *(firstFree + s) = -s;

                int newBlockPtr = firstFree + s + 2;
                *(newBlockPtr + size) = size;
                *(newBlockPtr - 1) = size;
                int LeftBlockPtr = *firstFree;
                int RightBlockPtr = *(firstFree + blockSize - 1);
                /*putchar(82);
                printf(RightBlockPtr);*/

                if(LeftBlockPtr)
                {
                    int LeftBlockSize = *(LeftBlockPtr - 1);
                    *(LeftBlockPtr + LeftBlockSize - 1) = newBlockPtr;
                }
                else
                    *0 = newBlockPtr;

                if(RightBlockPtr)
                {
                    *RightBlockPtr = newBlockPtr;
                }
                return firstFree;
            }
        }
        firstFree = *(firstFree + -blockSize -1);
    } while (firstFree != 0);
    return 0;
    //Error TODO
}

int free(int p)
{
    /*int size = *(p-1);
    if (size < 0 || size != *(p+size))
        int err = 1/0;
        //Error TODO
        
    *(p - 1) = -size;
    *(p + size) = -size;

    *p = 0;
    *(p + size - 1) = *0;
    **0 = p + size - 1;
    *0 = p;

    if (*(p + size + 2) < 0) // right block free
    {
        int ptrR = p + size + 1;
        int newSize = size + *(ptrR - 1) + 2;
        *(p - 1) = newSize;
        *(p + newSize) = newSize;
        size = newSize;
    }
    if (*(p - 2) < 0) // left block free
    {
        int ptrL = *(p - *());
        int newSize = size + *(ptrL - 1) + 2;
        *(p - 1) = newSize;
        *(p + newSize) = newSize;
        size = newSize;
    }*/
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
