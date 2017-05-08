#include <stdio.h>


int main() {
  int b = 0;

  while (b == 0) { 
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
        b = 1;
        printf("default a\n");
    }
  }
  return 0;
}
