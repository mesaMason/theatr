#include <stdio.h>

/* define structs which contain the argument for the receive functions */
typedef struct eat_arg {
  int weight;
} eat_arg;

typedef struct follow_me_arg {
  int placeholder;
} follow_me_arg;

/* define a union of the arguments. A message will contain one of these argument */
typedef union arg {
  eat_arg e;
  follow_me_arg f;
} dolphin_receive_args;


/* define the set of functions dolphin needs to execute */
typedef union funcs {
  void (*eat)(eat_arg);
  int  (*follow_me)(follow_me_arg);
} dolphin_receive_funcs;

/* define the message type that would be sent to a dolphin actor */
typedef struct dolphin_message {
  int message_type; /* chooses what function to execute, see details below */
  dolphin_receive_funcs f;
  dolphin_receive_args a;
} dolphin_message;


void dolphin_actor(dolphin_message m) {
  /* chooses the function to execute depending on the message type */
  switch(m.message_type) {
  case 1:
    printf("executing eat\n");
    m.f.eat(m.a.e);
    break;
  case 2:
    printf("executing follow me\n");
    m.f.follow_me(m.a.f);
    break;
  default:
    printf("Wrong Message");
  }
}

/* implement the functions for dolphin actor. The signature should match the 
signature defined in dophin_receive_funcs */
void dolphin_eat(eat_arg a) {
  printf("dolphin is eating\n");
}

int dolphin_follow_me(follow_me_arg a) {
  printf("dolphin is following\n");
}

int main() {

  dolphin_message m1;
  dolphin_message m2;

  m1.message_type = 1;
  m1.f.eat = &dolphin_eat;
  eat_arg ea = { 10 };
  m1.a.e = ea;

  m2.message_type = 2;
  m2.f.follow_me = &dolphin_follow_me;
  follow_me_arg fa = { 0 };
  m2.a.f = fa;
  
  dolphin_actor(m1);
  dolphin_actor(m2);
  dolphin_actor(m1);
  
  return 0;
}
