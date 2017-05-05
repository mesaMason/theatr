#include <stdio.h>
#include "queue.h"

int main() {
  head qhead = initialize_queue();
  message_t message, message2;
  message_t *retmessage;  
  message.val = 10;
  message2.val = 20;
  enqueue(&qhead, &message);
  enqueue(&qhead, &message2);
  retmessage = dequeue(&qhead);
  printf("%d\n",retmessage->val);
  retmessage = dequeue(&qhead);
  printf("%d\n",retmessage->val);
  return 0;
}
