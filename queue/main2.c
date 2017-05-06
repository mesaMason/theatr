#include <stdio.h>
#include "queue.h"

/* gcc -c main2.c && gcc -c queue.c && gcc -o main2 main2.o queue.o && ./main2 */ 
int main() {
  head qhead = initialize_queue();
  message_t message, message2;
  message_t retmessage;
  message.val = 10;
  message2.val = 20;
  enqueue(&qhead, message);
  enqueue(&qhead, message2);
  retmessage = dequeue(&qhead);
  printf("%d\n",retmessage.val);
  return 0;
}
