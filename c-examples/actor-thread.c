#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>

struct dolphin_state {
  int height;
  int weight;
};

struct trainer_state {
  int age;
};

/*
dolphin.eat(int *weight, int *age, food) {
    *weight = *weight + food;
}

dolphin(int weight, int age):
    int weight
    int age
    receive:
        eat(int food):
            weight = weight + food
    drop:
        return;
    after:
        return;

trainer(int age):
    int age
    receive:
        ...
*/
void *dolphin_actor(void *ptr)
{
  // copy state variables from ptr, location of msg queue
  struct dolphin_state *s = ptr;
  int height = s->height;
  int weight = s->weight;
  while (true) {
    // check msg queue
    // if msg found, full down and execute
    height++; // temp code here
  }
}

void *trainer_actor(void *ptr)
{
  // copy state variables from ptr, location of msg queue
  struct state *s = ptr;
  int age = s->age;
  while (true) {
    // check msg queue
    // if msg found, full down and execute
    age++; // temp code here
  }
}

int main()
{
  struct dolphin_state sd;
  sd.height = 5;
  sd.weight = 42;
  pthread_t tid;
  pthread_create(&tid, NULL, dolphin_actor, &sd);

  struct trainer_state st;
  st.age = 80;
  pthread_t tid_trainer;
  pthread_create(&tid_trainer, NULL, trainer_actor, &st);

  pthread_join(tid, NULL);
  pthread_join(tid_trainer, NULL);

  exit(0);
}

