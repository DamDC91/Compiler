// https://www.programiz.com/c-programming/examples/sum-prime-numbers

// function to check prime number
int checkPrime(int n) {
  int i, isPrime = 1;

  // 0 and 1 are not prime numbers
  if (n == 0 || n == 1) {
    isPrime = 0;
  }
  else {
    for(i = 2; i <= n/2; i=i+1) {
      if(n % i == 0) {
        isPrime = 0;
        break;
      }
    }
  }

  return isPrime;
}


int main() {
  int n, i, flag = 0;
  n = scanf();

  for (i = 2; i <= n / 2; i=i+1) {
    // condition for i to be a prime number
    if (checkPrime(i) == 1) {
      // condition for n-i to be a prime number
      if (checkPrime(n - i) == 1) {
        printf(i);
        printf(n-i);
        flag = 1;
      }
    }
  }

  if (flag == 0)
    printf(-1);

  return 0;
}

