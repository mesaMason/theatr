#ifndef QUEUE_H
#define QUEUE_H
#include <pthread.h>


typedef struct message {
  int val;
} message_t;

typedef struct queue {
  struct queue *next;
  message_t *message;
} queue_t;

typedef struct head {
  queue_t *queue;
  int count;
  pthread_cond_t count_cond;/*used to signal count = 1 after count goes to 0*/ 
  pthread_mutex_t lock;
} head;

head initialize_queue();

void enqueue(head *qhead, message_t *message);

message_t *dequeue(head *qhead);

void print_list(head *qhead);

#endif
