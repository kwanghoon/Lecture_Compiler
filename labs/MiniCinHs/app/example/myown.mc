/*
test
*/

int global = 3;

void main()
{
	int a, b;

	read(a);
	read(b);

	write(add(a, b));
}

int add(int a, int b)
{
	return a + b + 1;
}