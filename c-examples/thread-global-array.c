#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>

struct Books {
    int   book_id;
  int author_id;
};
int MAX_ACTORS = 1024;

typedef struct current_actors {
  int alive;
  void *msgQueue;
  pthread_t pid;
} currActors;

currActors actors[1024];

void *dolphin2(void *ptr) {
  struct Books *bk = ptr;
  int bookid = bk->book_id;
  int authorid = bk->author_id;
  free(bk);
  printf("My book_id is %d\n", bookid);
  printf("My author_id is %d\n", authorid);
  return;
}

void *dolphin(void *ptr)
{
  struct Books *bk = ptr;
  printf("My book_id is %d\n", bk->book_id);
  printf("My author_id is %d\n", bk->author_id);

  struct Books *b2 = malloc(sizeof(struct Books));
  b2->book_id = 500;
  b2->author_id = 4200;
  pthread_t tid2;
  pthread_create(&tid2, NULL, dolphin2, b2);
  actors[1].pid = tid2;
  return NULL;
}

int main()
{
  struct Books b1;
  b1.book_id = 5;
  b1.author_id = 42;
  pthread_t tid;
  pthread_create(&tid, NULL, dolphin, &b1);
  actors[0].pid = tid;
  sleep(1);
  pthread_t tid2;
  struct Books b3;
  b3.book_id = 20;
  b3.author_id = 55;
  pthread_create(&tid2, NULL, dolphin, &b3);
  actors[2].pid = tid2;
  for (int i = 0; i < MAX_ACTORS; i++) {
    printf("Joining %d\n", actors[i].pid);
    pthread_join(actors[i].pid, NULL);
  }
  return 0;
}


