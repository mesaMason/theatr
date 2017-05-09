#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "queue.h"

head *initialize_queue() {
  int ret;
  head *qhead = (head *)malloc(sizeof(head));

  qhead->queue = NULL;
  qhead->count = 0;

  ret = pthread_mutex_init(&qhead->lock, NULL);
  if (ret)
    perror("Error in initializing the queue lock");
  
  ret = pthread_cond_init(&qhead->count_cond, NULL);
  if (ret)
    perror("Error in initializing the count wait variable");
  printf("initialize: qhead @ %p\n", qhead);
  return qhead;
}
  
void enqueue(head *qhead, message_t message) {
  printf("enqueuing\n");
  queue_t *new_queue = malloc(sizeof(queue_t));
  if (!new_queue) return;

  new_queue->message = message;
  pthread_mutex_lock(&qhead->lock);
  qhead->count++;
  if (qhead->count == 1)
    pthread_cond_signal(&qhead->count_cond);
  new_queue->next = qhead->queue; 
  qhead->queue = new_queue;
  printf("enqueue: qhead count = %d\n", qhead->count);
  printf("enqueue: qhead @ %p\n", qhead);
  pthread_mutex_unlock(&qhead->lock);
}

message_t dequeue(head *qhead) {
  queue_t *current, *prev = NULL;
  message_t retmessage;

  printf("dequeueing\n");
  printf("dequeue: qhead @ %p\n", qhead);
  pthread_mutex_lock(&qhead->lock);

  while (qhead->count == 0){
    printf("dequeue: waiting on condition variable, qhead count = %d\n", qhead->count);
    pthread_cond_wait(&qhead->count_cond, &qhead->lock);
  }
  current = qhead->queue;

  while (current->next != NULL) {
    //    printf("curr->next is null, stepping...\n");
    prev = current;
    current = current->next;
  }

  retmessage = current->message;
  free(current);

  if (prev)
    prev->next = NULL;
  else
    qhead->queue = NULL;
  
  qhead->count--;
  pthread_mutex_unlock(&qhead->lock);
  return retmessage;
}
/* we are not deallocating the queue. The programmer is supposed to take care of that. */  
