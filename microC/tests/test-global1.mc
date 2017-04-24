int a;
int b;

void printa()
{
  print(a);
}

void printb()
{
  print(b);
}

void incab()
{
  a = a + 1;
  b = b + 1;
}

int main()
{
  b = 21;
  printa();
  a = 42;
  printb();
  incab();
  printa();
  printb();
  return 0;
}
