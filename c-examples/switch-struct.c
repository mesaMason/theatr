#include <stdio.h>
#include <stdlib.h>

struct foo {
  int caseNum;
  void *genericFuncArgs;
  void *sender;
};

int main() {
  struct foo original;
  original.caseNum = 44;
  original.genericFuncArgs = malloc(sizeof(int));
  original.sender = malloc(sizeof(int));

  void *local_msgp = &original;
  struct foo *insideSwitchCastedStruct;
  int caseNum;
  void *funcArgsStruct;
  void *sender;
  insideSwitchCastedStruct = (struct foo *)local_msgp;
  caseNum = insideSwitchCastedStruct->caseNum;
  funcArgsStruct = insideSwitchCastedStruct->genericFuncArgs;
  sender = insideSwitchCastedStruct->sender;

  int case_n = -1;
  switch(case_n) {
  case -1:
    break;
  case 0:
    break;
  default:
    break;
  }
  return 0;
}
