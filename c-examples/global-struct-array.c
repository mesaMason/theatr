#include <stdio.h>

struct actor_address_struct {
  int alive;
  void *msgQueue;
};

int MAX_ACTORS = 1024;

struct actor_address_struct global_actor_array[1024];
  
int main() {
  struct actor_address_struct a;
  a.alive = 0;
  a.msgQueue = NULL;
  for (int i = 0; i < MAX_ACTORS; i++) {
    global_actor_array[i] = a;
  }
  return 0;
}
