#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "queue.h"

head initialize_queue() {
  int ret;
  head qhead;
  qhead.queue = NULL;
  ret = pthread_mutex_init(&qhead.lock, NULL);
  if (ret)
    perror("Error in initializing the queue lock");
  return qhead;
}
  
void enqueue(head *qhead, message_t *message) {
  queue_t *new_queue = malloc(sizeof(queue_t));
  if (!new_queue) return;

  new_queue->message = message;
  // initializing the qhead.queue to null ensures that the first queue entry points to null
  pthread_mutex_lock(&qhead->lock);
  new_queue->next = qhead->queue; 
  qhead->queue = new_queue;
  pthread_mutex_unlock(&qhead->lock);
}

message_t *dequeue(head *qhead) {
  queue_t *current, *prev = NULL;
  message_t *retmessage;

  pthread_mutex_lock(&qhead->lock);
  if (qhead->queue == NULL) return NULL;
  current = qhead->queue;
  while (current->next != NULL) {
    prev = current;
    current = current->next;
  }

  retmessage = current->message;
  free(current);

  if (prev)
    prev->next = NULL;
  else
    qhead->queue = NULL;
  pthread_mutex_unlock(&qhead->lock);
  return retmessage;
}

void print_list(head *qhead) {
  queue_t *current = qhead->queue;

  while (current != NULL) {
    printf("%d\n", current->message->val);
    current = current->next;
  }
}