#include <stdio.h>


int main() {
  int a;
  a = 1;
  switch(a) {
  case -1:
    printf("a is -1\n");
  case 0:
    printf("a is 0\n");
  case 1:
    printf("a is 1\n");
  default:
    printf("default a\n");
  }
  return 0;
}
