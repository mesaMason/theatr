#include <stdio.h>


int main() {
  int b = 0;

  while (b == 0) { 
    printf("entered while loop\n");
    int case_n = -1;
    int a = 5;

    switch(case_n) {
    case -1:
        printf("-1: case_n is %d\n",case_n);
    case 0:
        a++;
        printf("0: case_n is %d\n", case_n);
    case 1:
        printf("1: case_n is %d\n", case_n);
    default:
        printf("default case_n is %d\n", case_n);
    }

    case_n = case_n + 1;
    if (case_n == 4) {
        b = 1;
    }
  }
  return 0;
}
