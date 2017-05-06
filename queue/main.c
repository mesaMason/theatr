/*
  To run this code do: 
  gcc -c queue.c && gcc -c main.c && gcc -o main main.o queue.o -lpthread && ./main 

*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "queue.h"

#define NUM_THREADS 10
/* argument would contain the queue head and dequeue function pointer */

typedef struct actor_info {
  head *qhead;
  int thread_id;
} actor_info_t;

void *dequeue_actor(void *t) {
  message_t retmessage;
  actor_info_t *info = (actor_info_t *) t;
  head *qhead = info->qhead;
  while(1) {
    retmessage = dequeue(qhead);
    printf("Message received by thread %d: val = %d\n", info->thread_id, retmessage.val);
    sleep(1);
  }
}

void *enqueue_actor(void *t) {
  message_t sendmessage;
  actor_info_t *info = (actor_info_t *) t;
  head *qhead = info->qhead;
  sendmessage.val = info->thread_id;
  enqueue(qhead, sendmessage);
  printf("Message sent by thread_id %d\n", info->thread_id);
}

int main() {
  int i, rc;
  head qhead = initialize_queue();
  pthread_t taskids[NUM_THREADS+1];
  actor_info_t actor_data_array[NUM_THREADS+1];
  
  for (i = 0; i < NUM_THREADS+1; i++) {

    actor_data_array[i].qhead = &qhead;
    actor_data_array[i].thread_id = i;

    if (i == 0)
      rc = pthread_create(&taskids[i], NULL, dequeue_actor, (void *)&actor_data_array[i]);
    else
      rc = pthread_create(&taskids[i], NULL, enqueue_actor, (void *)&actor_data_array[i]);

    if (rc) {
      printf("ERROR: return code from pthread_create() is %d\n", rc);
      exit(-1);
    }
  }

  for (i = 0; i < NUM_THREADS+1; i++)
    pthread_join(taskids[i], NULL);
    
  return 0;
}
