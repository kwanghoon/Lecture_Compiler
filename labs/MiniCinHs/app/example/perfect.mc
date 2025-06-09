/*
Exercise 2.18
*/

// const int max = 500;
int max;

void main()
{
    int i, j, k;
    int rem, sum; // rem: remainder

    max = 500;
    
    i = 2;
    while (i <= max) {
        sum = 0;
        k = i / 2;
        j = 1;
        while (j <= k) {
            rem = i % j;
            if (rem == 0) sum += j;
            ++j;
        }
        if (i == sum) write(i);
        ++i;
    }
}
