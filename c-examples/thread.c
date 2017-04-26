#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>

struct Books {
    int   book_id;
};

void *dolphin(void *ptr)
{
  struct Books *bk = ptr;
    
  printf("Hello from thread\n");
  printf("My book_id is %d\n", bk->book_id);
  return NULL;
}

int main()
{
  struct Books b1;
  b1.book_id = 25;
    
  pthread_t tid;
  pthread_create(&tid, NULL, dolphin, &b1);
  pthread_join(tid, NULL);
  exit(0);
}

