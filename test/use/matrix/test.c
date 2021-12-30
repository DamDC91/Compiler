// https://www.programiz.com/c-programming/examples/matrix-multiplication-function

int display(int mult, int rowFirst, int columnSecond)
{
	int i, j;
	for(i = 0; i < rowFirst; i=i+1)
	{
		for(j = 0; j < columnSecond; j=j+1)
		{
            printf(mult[i * columnSecond + j]);
		}
	}
}

int enterData(int firstMatrix, int secondMatrix, int rowFirst, int columnFirst, int rowSecond, int columnSecond)
{
	int i, j;
	for(i = 0; i < rowFirst; i=i+1)
	{
		for(j = 0; j < columnFirst; j=j+1)
		{
			firstMatrix[i * columnFirst + j] = scanf();
		}
	}


	for(i = 0; i < rowSecond; i=i+1)
	{
		for(j = 0; j < columnSecond; j=j+1)
		{
			secondMatrix[i * columnSecond + j] = scanf();
		}
	}
}

int multiplyMatrices(int firstMatrix, int secondMatrix, int mult, int rowFirst, int columnFirst, int rowSecond, int columnSecond)
{
	int i, j, k;

	// Initializing elements of matrix mult to 0.
	for(i = 0; i < rowFirst; i=i+1)
	{
		for(j = 0; j < columnSecond; j=j+1)
		{
			mult[i * columnFirst + j] = 0;
		}
	}

	// Multiplying matrix firstMatrix and secondMatrix and storing in array mult.
	for(i = 0; i < rowFirst; i=i+1)
	{
		for(j = 0; j < columnSecond; j=j+1)
		{
			for(k = 0; k<columnFirst; k=k+1)
			{
				mult[i * columnSecond + j] = mult[i * columnSecond + j] + firstMatrix[i * columnFirst + k] * secondMatrix[k * rowFirst + j];
			}
		}
	}
}



int main()
{
	int firstMatrix = malloc(100);
    int secondMatrix = malloc(100);
    int mult = malloc(100);
    int rowFirst, columnFirst, rowSecond, columnSecond, i, j, k;

	rowFirst = scanf();
	columnFirst = scanf();

	rowSecond = scanf();
	columnSecond = scanf();


	// Function to take matrices data
    enterData(firstMatrix, secondMatrix, rowFirst, columnFirst, rowSecond, columnSecond);

    // Function to multiply two matrices.
    multiplyMatrices(firstMatrix, secondMatrix, mult, rowFirst, columnFirst, rowSecond, columnSecond);

    // Function to display resultant matrix after multiplication.
    display(mult, rowFirst, columnSecond);

    free(firstMatrix);
    free(secondMatrix);
    free(mult);

	return 0;
}
