#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>

void switch_availability(int, int *, bool *, bool);
void *greeter(int);

int mainInt = 0;
bool cmd = false;

void *greeter(int count)
{
    sleep(1);
    bool is_available = true;
    
    int local_c = 2;
    int *count_r = &count;
    bool *is_available_r = &is_available;
    
    /*switch_availability(local_c, count_r, is_available_r, true);*/
    
    if(is_available)
    {
        printf("Hello, my count is %d\n", count);
    }
    else 
    {
        printf("Sorry not available\n");
    }

    return NULL;
}

void switch_availability(int local_c, int *local1_r, bool *local2_r, bool is_available)
{   
    *local2_r = (is_available && cmd);
    printf("is_available is now %d\n", *local2_r);
}

void *run(int i) {
    while(!cmd) {
        printf("In %d.\n", i);
    }
}

int main()
{

  pthread_t tid;
  pthread_create(&tid, NULL, greeter, 1);
  pthread_join(tid, NULL);

  /*pthread_t tid2;
  pthread_create(&tid2, NULL, greeter, 2);
  pthread_join(tid2, NULL);

  pthread_t p1, p2;
  pthread_create(&p1, NULL, run, 3);
  pthread_create(&p2, NULL, run, 4);
  pthread_join(p1, NULL);
  pthread_join(p2,NULL);
  cmd = true;
  */
  
  exit(0);
}

