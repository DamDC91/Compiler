{
    int a;
    a= 32;
    if (a > 0) 
    {
        if (a > 20)
        {
            debug 1;
            if (a > 30)
            {
                if (a > 40)
                {
                    debug 2;
                }
                else if (a == 31)
                {
                    debug 3;
                }
                else
                {
                    debug 4;
                }
            }
            else
            {
                debug 4;
            }
        }
        debug 6;
    }
    debug 7;
}
