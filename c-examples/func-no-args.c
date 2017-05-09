#include <stdlib.h>
#include <stdio.h>

struct bar {
  int a;
  int b;
};

struct bar* foo() {
  struct bar *a = malloc(sizeof(struct bar));
  a->a = 1;
  a->b = 1;
  return a;
}

int main() {
  struct bar *a;
  a = foo();
  printf("%d\n", a->a);
  return 0;
}
