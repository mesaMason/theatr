#include <stdio.h>
#include <stdlib.h>
struct foo {
  int a;
};

struct bar {
  int a;
  int b;
};
  
int main() {
  int b = 0;



  while (b == 0) { 
    printf("entered while loop\n");
    int case_n = -1;
    int a = 5;
    void *var = malloc(sizeof(struct bar));
    struct bar *castBar;
    struct foo *castFoo;
    switch(case_n) {
    case -1:
      castFoo = (struct foo *)var;
      castFoo->a = 42;
      printf("-1: case_n castedFoo is %d\n",castFoo->a);
      break;
    case 0:
      a++;
      printf("0: case_n castedBar is %d\n", case_n);
      break;
    case 1:
      printf("1: case_n is %d\n", case_n);
      break;
    default:
      printf("default case_n is %d\n", case_n);
      break;
    }

    case_n = case_n + 1;
    if (case_n == 4) {
        b = 1;
    }
  }
  return 0;
}
