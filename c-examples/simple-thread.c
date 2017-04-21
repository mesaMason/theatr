#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
int mainInt = 0;
// A normal C function that is executed as a thread when its name
// is specified in pthread_create()
void *dolphin(void *vargp)
{
  sleep(1);
  int weight = 1;
  int arg1 = 10;
  printf("Hello from thread, mainInt = %d\n", mainInt);
  eat(arg1, weight);
  return NULL;
}

void eat(int arg1, int weight) {
  weight = arg1 + weight;
  printf("weight is %d\n", weight);
}

void *createActor(void *vargp) {
  int msgQueue = 0;
  

}

int main()
{

  pthread_t tid;
  printf("Before Thread\n");
  pthread_create(&tid, NULL, dolphin, NULL);
  pthread_join(tid, NULL);
  printf("After Thread\n");
  exit(0);
}

